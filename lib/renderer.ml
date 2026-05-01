type artifact = {
  path : string;
  filename : string;
  content_type : string;
}

type rendered = {
  artifacts : artifact list;
  diagnostics : string;
  more_pages : int;
}

type import_file = { filename : string; contents : string }

let mkdir_p path =
  let rec loop path =
    if path = "" || path = Filename.dirname path || Sys.file_exists path then ()
    else (
      loop (Filename.dirname path);
      Unix.mkdir path 0o755)
  in
  loop path

let unique_dir root =
  mkdir_p root;
  let rec attempt n =
    let path =
      Filename.concat root
        (Printf.sprintf "job-%d-%06d" (Unix.getpid ()) (Random.int 1_000_000 + n))
    in
    if Sys.file_exists path then attempt (n + 1)
    else (
      Unix.mkdir path 0o700;
      path)
  in
  attempt 0

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let write_imports dir imports =
  List.iter
    (fun { filename; contents } ->
      write_file (Filename.concat dir filename) contents)
    imports

let looks_like_document source =
  let trimmed = String.trim source in
  String.starts_with ~prefix:"@require:" trimmed
  || String.starts_with ~prefix:"@import:" trimmed
  || String.starts_with ~prefix:"document" trimmed
  || String.contains trimmed '\''

let wrap_inline source =
  "@require: stdja\n\n"
  ^ "document (|\n"
  ^ "  title = {Slack SATySFi};\n"
  ^ "  author = {};\n"
  ^ "  show-title = false;\n"
  ^ "  show-toc = false;\n"
  ^ "|) '<\n"
  ^ "  +p {\n"
  ^ source
  ^ "\n  }\n"
  ^ ">\n"

let source_for_code source =
  if looks_like_document source then source else wrap_inline source

let satysfi_args config pdf saty =
  let config_args =
    match config.Config.satysfi_config_paths with
    | None | Some "" -> []
    | Some paths -> [ "-C"; paths ]
  in
  config_args @ [ "--full-path"; "-o"; pdf; saty ]

let collect_pngs max_pages dir =
  let files =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun name ->
           Filename.check_suffix name ".png"
           && String.length name >= 5
           && String.sub name 0 4 = "page")
    |> List.sort String.compare
  in
  let images, rest =
    let rec take n acc = function
      | [] -> (List.rev acc, [])
      | xs when n = 0 -> (List.rev acc, xs)
      | x :: xs -> take (n - 1) (Filename.concat dir x :: acc) xs
    in
    take max_pages [] files
  in
  (images, List.length rest)

let render ?(imports = []) (config : Config.t) (request : Command.render_request) =
  let dir = unique_dir config.work_dir in
  let saty = Filename.concat dir "input.saty" in
  let pdf = Filename.concat dir "output.pdf" in
  write_imports dir imports;
  write_file saty (source_for_code request.code);
  let satysfi =
    Process.run_capture ~cwd:dir ~timeout:config.render_timeout_secs config.satysfi_command
      (satysfi_args config pdf saty)
  in
  match Process.ensure_success ~what:"satysfi" satysfi with
  | Error msg ->
      let structured =
        satysfi.stdout ^ "\n" ^ satysfi.stderr
        |> Diagnostics.parse_satysfi_output |> Diagnostics.format_diagnostics
      in
      if structured = "SATySFi failed without a structured diagnostic." then
        Error (Code_block.truncate 3800 msg)
      else Error structured
  | Ok _ ->
      let ppm =
        Process.run_capture ~timeout:config.render_timeout_secs config.pdftoppm_command
          [ "-png"; "-r"; "144"; "-f"; "1"; "-l"; string_of_int config.max_pages; pdf; Filename.concat dir "page" ]
      in
      (match Process.ensure_success ~what:"pdftoppm" ppm with
      | Error msg -> Error (Code_block.truncate 3800 msg)
      | Ok _ ->
          let images, more_pages = collect_pngs config.max_pages dir in
          let artifacts =
            images
            |> List.mapi (fun i path ->
                   { path; filename = Printf.sprintf "page-%d.png" (i + 1); content_type = "image/png" })
          in
          Ok { artifacts; diagnostics = satysfi.stderr; more_pages })
