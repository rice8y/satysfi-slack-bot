type state = {
  config : Config.t;
  slack : Slack_client.t;
  seen : (string, unit) Hashtbl.t;
  lock : Mutex.t;
}

let response status body : Http_server.response = { status; body }

let ( let* ) = Result.bind

let header req name = Http_server.header req name

let verify_request state req =
  let now = int_of_float (Unix.time ()) in
  Slack_request.verify ~now ~secret:state.config.signing_secret
    ~timestamp:(header req "x-slack-request-timestamp")
    ~signature:(header req "x-slack-signature") ~body:req.Http_server.body ()

let target_of_event event =
  match (event.Slack_event.channel, event.ts) with
  | Some channel, Some thread_ts -> Some Slack_client.{ channel; thread_ts }
  | _ -> None

let help_text =
  "Commands:\n\
   - `@satysfi-bot render [format=png|pdf] <code block>`\n\
   - `@satysfi-bot r [fmt=png|pdf] <code block>`\n\
   - `@satysfi-bot version`\n\
   - `@satysfi-bot source`\n\n\
   Inline snippets are wrapped in a small SATySFi document. Full `.saty` documents are passed through."

let log_error context = function
  | Ok () -> ()
  | Error message -> Log.error "%s failed: %s" context message

let allowed_support_filename name =
  let base = Filename.basename name in
  base = name
  && String.length name > 0
  && not (String.contains name '/')
  && not (String.contains name ':')
  &&
  let lower = String.lowercase_ascii name in
  List.exists
    (fun suffix -> Filename.check_suffix lower suffix)
    [ ".satyh"; ".satyg"; ".png"; ".jpg"; ".jpeg"; ".pdf"; ".svg" ]

let support_name name =
  let name = Filename.basename name in
  if allowed_support_filename name then Ok name
  else
    Error
      (Printf.sprintf
         "unsupported support file `%s`; upload .satyh, .satyg, .png, .jpg, .jpeg, .pdf, or .svg files only"
         name)

let likely_support_filename name =
  let lower = String.lowercase_ascii name in
  List.exists
    (fun suffix -> Filename.check_suffix lower suffix)
    [ ".satyh"; ".satyg"; ".png"; ".jpg"; ".jpeg"; ".pdf"; ".svg" ]

let download_support_files state files =
  let rec loop seen acc = function
    | [] -> Ok (List.rev acc)
    | file :: rest -> (
        match (file.Slack_event.name, file.url_private_download) with
        | Some name, Some url -> (
            match support_name name with
            | Error msg -> Error msg
            | Ok filename ->
                if List.mem filename seen then
                  Error (Printf.sprintf "duplicate support filename `%s`" filename)
                else
                  let* contents =
                    Slack_client.download_private_file state.slack ~url
                      ~max_bytes:state.config.max_import_file_bytes
                  in
                  Log.debug "downloaded support file %s bytes=%d" filename
                    (String.length contents);
                  loop (filename :: seen)
                    (Renderer.{ filename; contents } :: acc)
                    rest)
        | Some name, None
          when likely_support_filename name ->
            Error
              (Printf.sprintf
                 "Slack did not include a private download URL for `%s`; add the files:read scope and reinstall the app"
                 name)
        | _ -> loop seen acc rest)
  in
  loop [] [] files

let post_error state target message =
  Slack_client.post_message state.slack target
    ("An error occurred:\n" ^ Diagnostics.slack_code_block message)
  |> log_error "chat.postMessage"

let handle_command state target files text =
  Log.debug "processing command text: %s attached_files=%d" text (List.length files);
  match Command.parse text with
  | Error err ->
      Slack_client.post_message state.slack target
        ("Command error: " ^ Command.string_of_error err)
      |> log_error "chat.postMessage"
  | Ok (Command.Help _) ->
      Slack_client.post_message state.slack target help_text |> log_error "chat.postMessage"
  | Ok Command.Source ->
      Slack_client.post_message state.slack target state.config.source_url
      |> log_error "chat.postMessage"
  | Ok Command.Version ->
      let result =
        Process.run_capture ~timeout:10. state.config.satysfi_command [ "--version" ]
      in
      let msg =
        match Process.ensure_success ~what:"satysfi --version" result with
        | Ok out -> String.trim out
        | Error err -> err
      in
      Slack_client.post_message state.slack target msg |> log_error "chat.postMessage"
  | Ok (Command.Unknown name) ->
      Slack_client.post_message state.slack target
        (Printf.sprintf "Unknown command `%s`. Use `@satysfi-bot help`." name)
      |> log_error "chat.postMessage"
  | Ok (Command.Render request) -> (
      Mutex.lock state.lock;
      let rendered =
        Fun.protect
          ~finally:(fun () -> Mutex.unlock state.lock)
          (fun () ->
            match download_support_files state files with
            | Error msg -> Error msg
            | Ok imports -> Renderer.render ~imports state.config request)
      in
      match rendered with
      | Error msg -> post_error state target msg
      | Ok rendered ->
          let notes = Buffer.create 128 in
          if rendered.artifacts = [] then Buffer.add_string notes "Note: no pages generated\n";
          if rendered.more_pages > 0 then
            Printf.bprintf notes "Note: %d more page(s) ignored\n" rendered.more_pages;
          if String.trim rendered.diagnostics <> "" then
            Printf.bprintf notes "SATySFi diagnostics:\n%s\n"
              (Diagnostics.slack_code_block rendered.diagnostics);
          let initial =
            let text = Buffer.contents notes |> String.trim in
            if text = "" then None else Some text
          in
          rendered.artifacts
          |> List.iteri (fun i artifact ->
                 let comment = if i = 0 then initial else None in
                 match Slack_client.upload_file state.slack target artifact ?initial_comment:comment () with
                 | Ok () -> ()
                 | Error msg -> post_error state target msg))

let process_event state event =
  Log.debug "event type=%s subtype=%s bot_id=%s" event.Slack_event.kind
    (Option.value event.subtype ~default:"")
    (Option.value event.bot_id ~default:"");
  if event.Slack_event.kind = "app_mention" && event.subtype = None && event.bot_id = None then
    match (target_of_event event, event.text) with
    | Some target, Some text -> (
        try handle_command state target event.files text
        with exn ->
          Log.error "command handling raised: %s\n%s"
            (Printexc.to_string exn) (Printexc.get_backtrace ()))
    | _ -> Log.debug "event missing channel/ts/text"

let handle_slack_events state req =
  match verify_request state req with
  | Error failure ->
      Log.error "Slack signature verification failed: %s"
        (Slack_request.string_of_failure failure);
      response 401 (Slack_request.string_of_failure failure)
  | Ok () -> (
      try
        match Slack_event.parse req.Http_server.body with
        | Url_verification challenge -> response 200 challenge
        | Unknown -> response 200 ""
        | Event_callback { event_id; event; _ } ->
            let first =
              if Hashtbl.mem state.seen event_id then false
              else (
                Hashtbl.add state.seen event_id ();
                true)
            in
            if first then ignore (Thread.create (fun () -> process_event state event) ());
            response 200 ""
      with exn -> response 400 (Printexc.to_string exn))

let handler state (req : Http_server.request) =
  match (req.meth, req.path) with
  | "GET", "/healthz" -> response 200 "ok"
  | "POST", "/slack/events" ->
      if String.length req.body > state.config.max_body_bytes then response 413 "body too large"
      else handle_slack_events state req
  | _ -> response 404 "not found"

let run () =
  Random.self_init ();
  let config = Config.load () in
  let state =
    {
      config;
      slack = Slack_client.create ~token:config.bot_token ~curl:config.curl_command;
      seen = Hashtbl.create 2048;
      lock = Mutex.create ();
    }
  in
  Http_server.serve ~host:config.bind_host ~port:config.bind_port
    ~max_body_bytes:config.max_body_bytes (handler state)
