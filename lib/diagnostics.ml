let zero_width_joiner = "\226\128\141"

let sanitize_code_block raw =
  let needle = "```" in
  let replacement = "``" ^ zero_width_joiner ^ "`" in
  let n = String.length raw in
  let rec loop pos acc =
    if pos >= n then List.rev acc
    else if pos + 3 <= n && String.sub raw pos 3 = needle then
      loop (pos + 3) (replacement :: acc)
    else loop (pos + 1) (String.make 1 raw.[pos] :: acc)
  in
  String.concat "" (loop 0 [])

let truncate limit text =
  if String.length text <= limit then text
  else
    let marker = "\n... truncated ...\n" in
    let keep = max 0 (limit - String.length marker) in
    String.sub text 0 keep ^ marker

let slack_code_block ?(limit = 1900) text =
  "```\n" ^ (text |> sanitize_code_block |> truncate limit) ^ "\n```"

let summarize_process_failure ~command ~exit_code ~stderr =
  let stderr_limit = 1200 in
  let stderr = stderr |> sanitize_code_block |> truncate stderr_limit in
  Printf.sprintf "SATySFi command failed (%s, exit %d)\n%s" command exit_code stderr

type severity = Error | Warning | Info

type span = {
  file : string;
  line : int;
  start_col : int;
  end_col : int;
}

type diagnostic = {
  severity : severity;
  phase : string;
  span : span option;
  message : string;
  details : string list;
}

let severity_of_phase phase =
  if Str.string_match (Str.regexp_case_fold ".*warning.*") phase 0 then Warning
  else if Str.string_match (Str.regexp_case_fold ".*error.*") phase 0 then Error
  else Info

let diagnostic_header = Str.regexp "^! \\[\\([^]]+\\)\\]\\(.*\\)$"

let location =
  Str.regexp
    ".*at \"\\([^\"]+\\)\", line \\([0-9]+\\), characters \
     \\([0-9]+\\)-\\([0-9]+\\):?\\(.*\\)$"

let parse_location text =
  if Str.string_match location text 0 then
    let file = Str.matched_group 1 text in
    let line = int_of_string (Str.matched_group 2 text) in
    let start_col = int_of_string (Str.matched_group 3 text) in
    let end_col = int_of_string (Str.matched_group 4 text) in
    let message = String.trim (Str.matched_group 5 text) in
    (Some { file; line; start_col; end_col }, message)
  else (None, String.trim text)

let finish current acc =
  match current with
  | None -> acc
  | Some diagnostic -> diagnostic :: acc

let parse_satysfi_output output =
  let current, diagnostics =
    output |> String.split_on_char '\n'
    |> List.fold_left
         (fun (current, diagnostics) line ->
           if Str.string_match diagnostic_header line 0 then
             let diagnostics = finish current diagnostics in
             let phase = String.trim (Str.matched_group 1 line) in
             let rest = String.trim (Str.matched_group 2 line) in
             let span, message = parse_location rest in
             let diagnostic =
               {
                 severity = severity_of_phase phase;
                 phase;
                 span;
                 message;
                 details = [];
               }
             in
             (Some diagnostic, diagnostics)
           else
             match current with
             | None -> (current, diagnostics)
             | Some diagnostic ->
                 let detail = String.trim line in
                 if detail = "" then (current, diagnostics)
                 else
                   ( Some
                       { diagnostic with details = diagnostic.details @ [ detail ] },
                     diagnostics ))
         (None, [])
  in
  List.rev (finish current diagnostics)

let string_of_severity = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"

let format_span = function
  | None -> ""
  | Some { file; line; start_col; end_col } ->
      Printf.sprintf " at %s:%d:%d-%d" file line start_col end_col

let format_one diagnostic =
  let title =
    Printf.sprintf "[%s: %s]%s"
      (string_of_severity diagnostic.severity)
      diagnostic.phase (format_span diagnostic.span)
  in
  let lines =
    if diagnostic.message = "" then title :: diagnostic.details
    else title :: diagnostic.message :: diagnostic.details
  in
  String.concat "\n" lines

let format_diagnostics ?(max_len = 1900) diagnostics =
  let text =
    match diagnostics with
    | [] -> "SATySFi failed without a structured diagnostic."
    | diagnostics -> diagnostics |> List.map format_one |> String.concat "\n\n"
  in
  truncate max_len text
