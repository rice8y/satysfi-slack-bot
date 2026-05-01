type target = { channel : string; thread_ts : string }

type t = { token : string; curl : string }

let create ~(token : string) ~(curl : string) = { token; curl }

let bearer t = "Authorization: Bearer " ^ t.token

let ( let* ) = Result.bind

let with_temp_payload body f =
  let path = Filename.temp_file "satysfi-slack-bot-slack-" ".json" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      let oc = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr oc)
        (fun () -> output_string oc body);
      f path)

let curl_json t url json =
  let body = Yojson.Safe.to_string json in
  Log.debug "Slack API POST %s payload_bytes=%d" url (String.length body);
  let result =
    with_temp_payload body (fun payload_path ->
        Process.run_capture ~timeout:45. t.curl
          [
            "-sS";
            "--show-error";
            "--http1.1";
            "--connect-timeout";
            "10";
            "--max-time";
            "30";
            "--ipv4";
            "-X";
            "POST";
            "-H";
            bearer t;
            "-H";
            "Content-Type: application/json; charset=utf-8";
            "--data-binary";
            "@" ^ payload_path;
            url;
          ])
  in
  match Process.ensure_success ~what:"curl" result with
  | Error msg ->
      Log.error "curl failed for %s: %s" url msg;
      Error msg
  | Ok stdout -> (
      Log.debug "Slack API response from %s: %s" url stdout;
      try Ok (Yojson.Safe.from_string stdout)
      with Yojson.Json_error msg -> Error ("invalid Slack JSON response: " ^ msg ^ "\n" ^ stdout))

let slack_ok method_name json =
  match Yojson.Safe.Util.(member "ok" json |> to_bool_option) with
  | Some true ->
      Log.debug "%s ok" method_name;
      Ok json
  | _ ->
      let error =
        Yojson.Safe.Util.(member "error" json |> to_string_option)
        |> Option.value ~default:"unknown_error"
      in
      Error (Printf.sprintf "%s failed: %s" method_name error)

let post_message t target text =
  Log.debug "chat.postMessage channel=%s thread_ts=%s text_bytes=%d"
    target.channel target.thread_ts (String.length text);
  let json =
    `Assoc
      [
        ("channel", `String target.channel);
        ("thread_ts", `String target.thread_ts);
        ("text", `String text);
        ("unfurl_links", `Bool false);
        ("unfurl_media", `Bool false);
      ]
  in
  let* json = curl_json t "https://slack.com/api/chat.postMessage" json in
  let* _ = slack_ok "chat.postMessage" json in
  Ok ()

let file_size path = (Unix.stat path).Unix.st_size

let download_private_file t ~url ~max_bytes =
  Log.debug "Slack private file download %s" url;
  let result =
    Process.run_capture ~timeout:45. t.curl
      [
        "-sS";
        "--show-error";
        "--http1.1";
        "--connect-timeout";
        "10";
        "--max-time";
        "30";
        "--ipv4";
        "-L";
        "-H";
        bearer t;
        url;
      ]
  in
  match Process.ensure_success ~what:"Slack private file download" result with
  | Error msg -> Error msg
  | Ok contents ->
      if String.length contents > max_bytes then
        Error
          (Printf.sprintf "uploaded import file is too large: %d bytes exceeds %d bytes"
             (String.length contents) max_bytes)
      else Ok contents

let upload_file t target (artifact : Renderer.artifact) ?initial_comment () =
  Log.debug "upload_file channel=%s thread_ts=%s file=%s bytes=%d"
    target.channel target.thread_ts artifact.Renderer.filename
    (file_size artifact.Renderer.path);
  let length = string_of_int (file_size artifact.Renderer.path) in
  let upload_url_response =
    Process.run_capture ~timeout:60. t.curl
      [
        "-sS";
        "--show-error";
        "--http1.1";
        "--connect-timeout";
        "10";
        "--max-time";
        "30";
        "--ipv4";
        "-X";
        "POST";
        "-H";
        bearer t;
        "--data-urlencode";
        "filename=" ^ artifact.filename;
        "--data-urlencode";
        "length=" ^ length;
        "--data-urlencode";
        "alt_txt=SATySFi rendered output";
        "https://slack.com/api/files.getUploadURLExternal";
      ]
  in
  match Process.ensure_success ~what:"files.getUploadURLExternal curl" upload_url_response with
  | Error msg -> Error msg
  | Ok stdout -> (
      Log.debug "Slack API response from files.getUploadURLExternal: %s" stdout;
      let json = Yojson.Safe.from_string stdout in
      match slack_ok "files.getUploadURLExternal" json with
      | Error msg -> Error msg
      | Ok json -> (
          let upload_url = Yojson.Safe.Util.(member "upload_url" json |> to_string) in
          let file_id = Yojson.Safe.Util.(member "file_id" json |> to_string) in
          let raw_upload =
            Process.run_capture ~timeout:120. t.curl
              [
                "-sS";
                "--fail";
                "--show-error";
                "--http1.1";
                "--connect-timeout";
                "10";
                "--max-time";
                "60";
                "--ipv4";
                "-X";
                "POST";
                "-H";
                "Content-Type: " ^ artifact.content_type;
                "--data-binary";
                "@" ^ artifact.path;
                upload_url;
              ]
          in
          match Process.ensure_success ~what:"Slack raw file upload" raw_upload with
          | Error msg -> Error msg
          | Ok _ ->
              let complete =
                `Assoc
                  [
                    ("channel_id", `String target.channel);
                    ("thread_ts", `String target.thread_ts);
                    ( "files",
                      `List
                        [
                          `Assoc
                            [
                              ("id", `String file_id);
                              ("title", `String artifact.filename);
                            ];
                        ] );
                  ]
              in
              let complete =
                match initial_comment with
                | None -> complete
                | Some comment -> (
                    match complete with
                    | `Assoc fields -> `Assoc (("initial_comment", `String comment) :: fields)
                    | _ -> complete)
              in
              let* json =
                curl_json t "https://slack.com/api/files.completeUploadExternal" complete
              in
              let* _ = slack_ok "files.completeUploadExternal" json in
              Ok ()))
