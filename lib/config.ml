type t = {
  bind_host : string;
  bind_port : int;
  bot_token : string;
  signing_secret : string;
  source_url : string;
  work_dir : string;
  satysfi_command : string;
  satysfi_config_paths : string option;
  pdftoppm_command : string;
  curl_command : string;
  max_body_bytes : int;
  max_import_file_bytes : int;
  max_pages : int;
  render_timeout_secs : float;
}

let read_file_trimmed path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len |> String.trim)

let secret name =
  let file_name = name ^ "_FILE" in
  match (Sys.getenv_opt name, Sys.getenv_opt file_name) with
  | Some value, None -> String.trim value
  | None, Some path -> read_file_trimmed path
  | Some _, Some _ -> failwith (Printf.sprintf "set only one of %s and %s" name file_name)
  | None, None -> failwith (Printf.sprintf "missing %s or %s" name file_name)

let getenv_default name default = Option.value (Sys.getenv_opt name) ~default

let getenv_int name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> int_of_string raw

let getenv_float name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some raw -> float_of_string raw

let parse_bind_addr raw =
  match String.rindex_opt raw ':' with
  | None -> failwith "BIND_ADDR must be host:port"
  | Some i ->
      let host = String.sub raw 0 i in
      let port = String.sub raw (i + 1) (String.length raw - i - 1) |> int_of_string in
      (host, port)

let load () =
  let bind_host, bind_port = getenv_default "BIND_ADDR" "0.0.0.0:3000" |> parse_bind_addr in
  {
    bind_host;
    bind_port;
    bot_token = secret "SLACK_BOT_TOKEN";
    signing_secret = secret "SLACK_SIGNING_SECRET";
    source_url =
      getenv_default "SATYSFI_BOT_SOURCE_URL" "https://github.com/yoneyama/satysfi-slack-bot";
    work_dir =
      getenv_default "SATYSFI_BOT_WORK_DIR"
        (Filename.concat (Filename.get_temp_dir_name ()) "satysfi-slack-bot");
    satysfi_command = getenv_default "SATYSFI_BOT_COMMAND" "satysfi";
    satysfi_config_paths = Sys.getenv_opt "SATYSFI_BOT_CONFIG_PATHS";
    pdftoppm_command = getenv_default "SATYSFI_BOT_PDFTOPPM_COMMAND" "pdftoppm";
    curl_command = getenv_default "SATYSFI_BOT_CURL_COMMAND" "curl";
    max_body_bytes = getenv_int "SATYSFI_BOT_MAX_BODY_BYTES" (1024 * 1024);
    max_import_file_bytes = getenv_int "SATYSFI_BOT_MAX_IMPORT_FILE_BYTES" (1024 * 1024);
    max_pages = getenv_int "SATYSFI_BOT_MAX_PAGES" 5;
    render_timeout_secs = getenv_float "SATYSFI_BOT_RENDER_TIMEOUT_SECS" 45.;
  }
