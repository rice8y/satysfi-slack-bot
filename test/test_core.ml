open Satysfi_slack_bot

let require label condition =
  if not condition then failwith ("assertion failed: " ^ label)

let equal_string label expected actual =
  if not (String.equal expected actual) then
    failwith (Printf.sprintf "%s\nexpected: %S\nactual:   %S" label expected actual)

let equal_result label expected actual =
  if expected <> actual then failwith ("unexpected result: " ^ label)

let test_sha256_and_hmac () =
  equal_string "sha256 abc"
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    (Sha256.sha256_hex "abc");
  equal_string "hmac sha256"
    "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"
    (Sha256.hmac_sha256_hex ~key:"key"
       ~data:"The quick brown fox jumps over the lazy dog")

let test_slack_request_verification () =
  let secret = "test-secret" in
  let timestamp = "1700000000" in
  let body = "token=x&team_id=T123&text=render+%60hello%60" in
  equal_string "signing base" ("v0:" ^ timestamp ^ ":" ^ body)
    (Slack_request.signing_base ~version:"v0" ~timestamp ~body);
  let signature = Slack_request.sign ~secret ~timestamp ~body in
  equal_result "valid signature" (Ok ())
    (Slack_request.verify ~now:1700000001 ~secret ~timestamp:(Some timestamp)
       ~signature:(Some signature) ~body ());
  equal_result "reject stale timestamp" (Error Slack_request.Timestamp_out_of_range)
    (Slack_request.verify ~now:1700001000 ~secret ~timestamp:(Some timestamp)
       ~signature:(Some signature) ~body ());
  equal_result "reject mismatched body" (Error Slack_request.Signature_mismatch)
    (Slack_request.verify ~now:1700000001 ~secret ~timestamp:(Some timestamp)
       ~signature:(Some signature) ~body:(body ^ "&tampered=1") ());
  equal_result "reject unsupported signature version"
    (Error (Slack_request.Unsupported_version "v1"))
    (Slack_request.verify ~now:1700000001 ~secret ~timestamp:(Some timestamp)
       ~signature:(Some "v1=abc") ~body ())

let test_command_parsing () =
  equal_result "missing command" (Error Command.Missing_command) (Command.parse "  ");
  equal_result "mentions are stripped"
    (Ok (Command.Render { format = Command.Png; code = "hello" }))
    (Command.parse "<@U123> render `hello`");
  equal_result "multiple mentions and alias"
    (Ok (Command.Render { format = Command.Pdf; code = "document" }))
    (Command.parse "<@U123> <@U456> r fmt=pdf ```document```");
  equal_result "command may be separated from block by newline"
    (Ok
       (Command.Render
          {
            format = Command.Png;
            code =
              "\n@require: stdjabook\n\n\
               document (|\n\
               \  title = {\\SATySFi;概説};\n\
               \  author = {Takashi SUWA};\n\
               \  show-title = true;\n\
               \  show-toc = true;\n\
               |) '<\n\
               \  +section{はじめに}<\n\
               \  >\n\
               >\n";
          }))
    (Command.parse
       "<@U123> render\n\
        ```\n\
        @require: stdjabook\n\n\
        document (|\n\
        \  title = {\\SATySFi;概説};\n\
        \  author = {Takashi SUWA};\n\
        \  show-title = true;\n\
        \  show-toc = true;\n\
        |) '<\n\
        \  +section{はじめに}<\n\
        \  >\n\
        >\n\
        ```");
  equal_result "Slack entities are decoded inside code blocks"
    (Ok
       (Command.Render
          {
            format = Command.Png;
            code = "@require: stdjabook\n|) '<\n  +section{はじめに}<\n  >\n>";
          }))
    (Command.parse
       "<@U123> render ```@require: stdjabook\n\
        |) '&lt;\n\
        \  +section{はじめに}&lt;\n\
        \  &gt;\n\
        &gt;```");
  equal_result "outer triple fence may contain inner triple fences"
    (Ok
       (Command.Render
          {
            format = Command.Png;
            code =
              "\n\
               document (| title = {T}; author = {}; show-title = false; show-toc = false; |) '<\n\
               \  +display-boxes('<\n\
               \    +p{body}\n\
               \  >)(```\n\
               \    +p{sample}\n\
               \  ```);\n\
               >\n";
          }))
    (Command.parse
       "render\n\
        ```\n\
        document (| title = {T}; author = {}; show-title = false; show-toc = false; |) '<\n\
        \  +display-boxes('<\n\
        \    +p{body}\n\
        \  >)(```\n\
        \    +p{sample}\n\
        \  ```);\n\
        >\n\
        ```");
  equal_result "keeps prose after code outside parsed source"
    (Ok (Command.Render { format = Command.Png; code = "let-inline ctx \\hello;" }))
    (Command.parse "render `let-inline ctx \\hello;` can anyone check this?");
  equal_result "help topic" (Ok (Command.Help (Some "render"))) (Command.parse "help render");
  equal_result "version" (Ok Command.Version) (Command.parse "<@BOT> version");
  equal_result "unknown command is explicit" (Ok (Command.Unknown "deploy"))
    (Command.parse "deploy now");
  equal_result "missing code block" (Error Command.Missing_code_block)
    (Command.parse "render fmt=png");
  equal_result "invalid format" (Error (Command.Invalid_format "jpeg"))
    (Command.parse "render format=jpeg `hello`");
  equal_result "unrecognized flag" (Error (Command.Unrecognized_flag "theme=dark"))
    (Command.parse "render theme=dark `hello`")

let test_diagnostics () =
  let sanitized = Diagnostics.sanitize_code_block "before ``` after" in
  require "sanitized contains no raw triple backticks"
    (not (String.contains_from sanitized 0 '`'
          && String.length sanitized >= 3
          && String.sub sanitized 7 3 = "```"));
  equal_string "sanitized exact" "before ``\226\128\141` after" sanitized;
  equal_string "code block wrapper" "```\nline 1\n```"
    (Diagnostics.slack_code_block "line 1");
  let limited = Diagnostics.slack_code_block ~limit:12 "12345678901234567890" in
  require "limit marker present" (String.contains limited '.');
  let failure =
    Diagnostics.summarize_process_failure ~command:"satysfi -o output.pdf input.saty"
      ~exit_code:1 ~stderr:"syntax ``` error"
  in
  require "process summary names SATySFi" (String.starts_with ~prefix:"SATySFi command failed" failure);
  require "process summary sanitizes backticks" (not (String.contains failure '\000'))

let () =
  test_sha256_and_hmac ();
  test_slack_request_verification ();
  test_command_parsing ();
  test_diagnostics ()
