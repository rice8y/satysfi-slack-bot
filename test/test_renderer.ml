open Satysfi_slack_bot

let require label condition =
  if not condition then failwith ("assertion failed: " ^ label)

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let with_temp_dir f =
  let marker = Filename.temp_file "satysfi-slack-bot-renderer-" "" in
  Sys.remove marker;
  Unix.mkdir marker 0o700;
  Fun.protect ~finally:(fun () -> ()) (fun () -> f marker)

let config ~work_dir ~satysfi_command ~pdftoppm_command =
  Config.
    {
      bind_host = "127.0.0.1";
      bind_port = 0;
      bot_token = "xoxb-test";
      signing_secret = "secret";
      source_url = "https://example.invalid/source";
      work_dir;
      satysfi_command;
      satysfi_config_paths = None;
      pdftoppm_command;
      curl_command = "curl";
      max_body_bytes = 1024 * 1024;
      max_import_file_bytes = 1024 * 1024;
      max_pages = 2;
      render_timeout_secs = 5.;
    }

let fake_satysfi_success =
  {|
#!/bin/sh
out=""
while [ "$#" -gt 0 ]; do
  if [ "$1" = "-o" ]; then
    shift
    out="$1"
  fi
  shift
done
echo "satysfi warning" >&2
printf "%s\n" "%PDF-1.7" > "$out"
|}

let fake_satysfi_failure =
  {|
#!/bin/sh
echo '! [Syntax Error at Parser] at "input.saty", line 2, characters 1-4: broken' >&2
exit 7
|}

let fake_pdftoppm =
  {|
#!/bin/sh
prefix=""
for arg in "$@"; do
  prefix="$arg"
done
printf "png1" > "${prefix}-1.png"
printf "png2" > "${prefix}-2.png"
printf "png3" > "${prefix}-3.png"
|}

let executable dir name contents =
  let path = Filename.concat dir name in
  write_file path contents;
  Unix.chmod path 0o755;
  path

let test_png_render_collects_pages () =
  with_temp_dir (fun dir ->
      let satysfi = executable dir "satysfi-ok" fake_satysfi_success in
      let pdftoppm = executable dir "pdftoppm-ok" fake_pdftoppm in
      let config = config ~work_dir:dir ~satysfi_command:satysfi ~pdftoppm_command:pdftoppm in
      match Renderer.render config Command.{ code = "+p {hello}" } with
      | Ok { Renderer.artifacts; diagnostics; more_pages } ->
          require "max_pages limits uploaded PNG artifacts" (List.length artifacts = 2);
          require "extra pages are counted" (more_pages = 1);
          require "satysfi stderr is preserved" (String.contains diagnostics 'w');
          List.iter
            (fun (artifact : Renderer.artifact) ->
              require "png content type" (artifact.content_type = "image/png"))
            artifacts
      | Error message -> failwith ("unexpected render failure: " ^ message))

let test_structured_failure () =
  with_temp_dir (fun dir ->
      let satysfi = executable dir "satysfi-fail" fake_satysfi_failure in
      let pdftoppm = executable dir "pdftoppm-unused" fake_pdftoppm in
      let config = config ~work_dir:dir ~satysfi_command:satysfi ~pdftoppm_command:pdftoppm in
      match Renderer.render config Command.{ code = "+p {bad}" } with
      | Error message ->
          require "structured diagnostic includes phase" (String.contains message 'P');
          require "structured diagnostic includes source location" (String.contains message '2')
      | Ok _ -> failwith "expected render failure")

let test_import_files_are_written_next_to_input () =
  with_temp_dir (fun dir ->
      let satysfi =
        executable dir "satysfi-check-import"
          {|
#!/bin/sh
out=""
input=""
while [ "$#" -gt 0 ]; do
  if [ "$1" = "-o" ]; then
    shift
    out="$1"
  else
    input="$1"
  fi
  shift
done
dir=$(dirname "$input")
test -f "$dir/my-lib.satyh" || exit 9
grep -q "let-inline" "$dir/my-lib.satyh" || exit 10
printf "%s\n" "%PDF-1.7" > "$out"
|}
      in
      let pdftoppm = executable dir "pdftoppm-ok" fake_pdftoppm in
      let config = config ~work_dir:dir ~satysfi_command:satysfi ~pdftoppm_command:pdftoppm in
      let imports =
        [ Renderer.{ filename = "my-lib.satyh"; contents = "let-inline ctx \\foo = {foo};" } ]
      in
      match Renderer.render ~imports config Command.{ code = "@import: my-lib" } with
      | Ok { Renderer.artifacts; _ } -> require "expected PNG artifacts" (artifacts <> [])
      | Error message -> failwith ("unexpected render failure: " ^ message))

let test_import_directive_is_not_wrapped_as_inline_text () =
  with_temp_dir (fun dir ->
      let satysfi =
        executable dir "satysfi-check-source"
          {|
#!/bin/sh
out=""
input=""
while [ "$#" -gt 0 ]; do
  if [ "$1" = "-o" ]; then
    shift
    out="$1"
  else
    input="$1"
  fi
  shift
done
head -n 1 "$input" | grep -q '^@import: my-lib$' || exit 11
printf "%s\n" "%PDF-1.7" > "$out"
|}
      in
      let pdftoppm = executable dir "pdftoppm-ok" fake_pdftoppm in
      let config = config ~work_dir:dir ~satysfi_command:satysfi ~pdftoppm_command:pdftoppm in
      match Renderer.render config Command.{ code = "@import: my-lib" } with
      | Ok { Renderer.artifacts; _ } -> require "expected PNG artifacts" (artifacts <> [])
      | Error message -> failwith ("unexpected render failure: " ^ message))

let () =
  test_png_render_collects_pages ();
  test_structured_failure ();
  test_import_files_are_written_next_to_input ();
  test_import_directive_is_not_wrapped_as_inline_text ()
