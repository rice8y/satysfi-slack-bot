type request = {
  meth : string;
  path : string;
  headers : (string * string) list;
  body : string;
}

type response = { status : int; body : string }

let status_text = function
  | 200 -> "OK"
  | 400 -> "Bad Request"
  | 401 -> "Unauthorized"
  | 404 -> "Not Found"
  | 413 -> "Payload Too Large"
  | 500 -> "Internal Server Error"
  | _ -> "OK"

let header request name =
  let name = String.lowercase_ascii name in
  List.assoc_opt name request.headers

let trim_cr s =
  let len = String.length s in
  if len > 0 && s.[len - 1] = '\r' then String.sub s 0 (len - 1) else s

let parse_header line =
  match String.index_opt line ':' with
  | None -> None
  | Some i ->
      let key = String.sub line 0 i |> String.lowercase_ascii in
      let value = String.sub line (i + 1) (String.length line - i - 1) |> String.trim in
      Some (key, value)

let read_request max_body_bytes fd =
  let ic = Unix.in_channel_of_descr fd in
  let first = input_line ic |> trim_cr in
  let meth, path =
    match String.split_on_char ' ' first with
    | meth :: path :: _ -> (meth, path)
    | _ -> failwith "invalid HTTP request line"
  in
  let rec headers acc =
    let line = input_line ic |> trim_cr in
    if line = "" then List.rev acc
    else
      match parse_header line with
      | None -> headers acc
      | Some header -> headers (header :: acc)
  in
  let headers = headers [] in
  let content_length =
    List.assoc_opt "content-length" headers |> Option.map int_of_string |> Option.value ~default:0
  in
  if content_length > max_body_bytes then
    { meth; path; headers; body = String.make (max_body_bytes + 1) 'x' }
  else
    let body = really_input_string ic content_length in
    { meth; path; headers; body }

let write_response fd response =
  let oc = Unix.out_channel_of_descr fd in
  let body = response.body in
  Printf.fprintf oc "HTTP/1.1 %d %s\r\n" response.status (status_text response.status);
  Printf.fprintf oc "Content-Type: text/plain; charset=utf-8\r\n";
  Printf.fprintf oc "Content-Length: %d\r\n" (String.length body);
  Printf.fprintf oc "Connection: close\r\n\r\n";
  output_string oc body;
  flush oc

let log_request request response =
  Log.debug "%s %s -> %d" request.meth request.path response.status

let inet_addr host =
  if host = "0.0.0.0" then Unix.inet_addr_any
  else if host = "127.0.0.1" || host = "localhost" then Unix.inet_addr_loopback
  else (Unix.gethostbyname host).Unix.h_addr_list.(0)

let serve ~host ~port ~max_body_bytes handler =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  (try Unix.bind socket (Unix.ADDR_INET (inet_addr host, port))
   with Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
     failwith
       (Printf.sprintf
          "address already in use: %s:%d; set BIND_ADDR to another host:port"
          host port));
  Unix.listen socket 64;
  Log.info "satysfi-slack-bot listening on %s:%d" host port;
  let rec loop () =
    let fd, _ = Unix.accept socket in
    let _thread =
      Thread.create
        (fun () ->
          Fun.protect
            ~finally:(fun () -> Unix.close fd)
            (fun () ->
              try
                let request = read_request max_body_bytes fd in
                let response = handler request in
                log_request request response;
                write_response fd response
              with exn ->
                let response = { status = 400; body = Printexc.to_string exn } in
                Log.error "bad request -> 400: %s" (Printexc.to_string exn);
                write_response fd response))
        ()
    in
    loop ()
  in
  loop ()
