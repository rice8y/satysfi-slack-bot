type output_format = Pdf | Png

type render_request = {
  format : output_format;
  code : string;
}

type t =
  | Render of render_request
  | Help of string option
  | Version
  | Source
  | Unknown of string

type error =
  | Missing_command
  | Missing_code_block
  | Unclosed_code_block
  | Invalid_format of string
  | Unrecognized_flag of string

let string_of_error = function
  | Missing_command -> "missing command"
  | Missing_code_block -> "missing SATySFi code block"
  | Unclosed_code_block -> "unclosed code block"
  | Invalid_format format -> "invalid render format: " ^ format
  | Unrecognized_flag flag -> "unrecognized render flag: " ^ flag

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let trim input =
  let len = String.length input in
  let left = ref 0 in
  while !left < len && is_space input.[!left] do
    incr left
  done;
  let right = ref (len - 1) in
  while !right >= !left && is_space input.[!right] do
    decr right
  done;
  if !right < !left then "" else String.sub input !left (!right - !left + 1)

let rec strip_leading_mentions input =
  let input = trim input in
  if String.length input >= 3 && input.[0] = '<' && input.[1] = '@' then
    match String.index_opt input '>' with
    | None -> input
    | Some close -> strip_leading_mentions (String.sub input (close + 1) (String.length input - close - 1))
  else input

let split_first_word input =
  let input = trim input in
  let rec find_space i =
    if i >= String.length input then None
    else if is_space input.[i] then Some i
    else find_space (i + 1)
  in
  match find_space 0 with
  | None -> (input, "")
  | Some index ->
      let word = String.sub input 0 index in
      let rest = String.sub input (index + 1) (String.length input - index - 1) in
      (word, trim rest)

let backtick_run input index =
  let count = ref 0 in
  while index + !count < String.length input && input.[index + !count] = '`' do
    incr count
  done;
  !count

let replace_all ~pattern ~with_ text =
  Str.global_replace (Str.regexp_string pattern) with_ text

let decode_slack_entities text =
  text
  |> replace_all ~pattern:"&lt;" ~with_:"<"
  |> replace_all ~pattern:"&gt;" ~with_:">"
  |> replace_all ~pattern:"&amp;" ~with_:"&"

let find_code_block input =
  let rec find_open i =
    if i >= String.length input then Ok None
    else if input.[i] = '`' then
      let ticks = backtick_run input i in
      let fence = String.make ticks '`' in
      if ticks >= 3 then find_last_close (i + ticks) fence ticks i None
      else find_close (i + ticks) fence ticks i
    else find_open (i + 1)
  and find_close i fence ticks open_index =
    if i > String.length input - ticks then Error Unclosed_code_block
    else if String.sub input i ticks = fence then
      let code = String.sub input (open_index + ticks) (i - open_index - ticks) in
      let before = String.sub input 0 open_index in
      Ok (Some (before, decode_slack_entities code))
    else find_close (i + 1) fence ticks open_index
  and find_last_close i fence ticks open_index last =
    if i > String.length input - ticks then
      match last with
      | None -> Error Unclosed_code_block
      | Some close_index ->
          let code =
            String.sub input (open_index + ticks) (close_index - open_index - ticks)
          in
          let before = String.sub input 0 open_index in
          Ok (Some (before, decode_slack_entities code))
    else if String.sub input i ticks = fence then
      find_last_close (i + ticks) fence ticks open_index (Some i)
    else find_last_close (i + 1) fence ticks open_index last
  in
  find_open 0

let parse_format = function
  | "pdf" -> Ok Pdf
  | "png" -> Ok Png
  | other -> Error (Invalid_format other)

let parse_flag request flag =
  match String.index_opt flag '=' with
  | None -> Error (Unrecognized_flag flag)
  | Some index -> (
      let key = String.sub flag 0 index in
      let value = String.sub flag (index + 1) (String.length flag - index - 1) in
      match key with
      | "format" | "fmt" ->
          parse_format value |> Result.map (fun format -> { request with format })
      | _ -> Error (Unrecognized_flag flag))

let parse_render args =
  match find_code_block args with
  | Error error -> Error error
  | Ok None -> Error Missing_code_block
  | Ok (Some (before, code)) ->
      let request = ref { format = Png; code } in
      let flags =
        before |> trim
        |> String.split_on_char ' '
        |> List.concat_map (String.split_on_char '\n')
        |> List.concat_map (String.split_on_char '\t')
        |> List.filter (fun s -> s <> "")
      in
      let rec apply = function
        | [] -> Ok (Render !request)
        | flag :: rest -> (
            match parse_flag !request flag with
            | Ok next ->
                request := next;
                apply rest
            | Error error -> Error error)
      in
      apply flags

let parse input =
  let command, args = input |> strip_leading_mentions |> split_first_word in
  match command with
  | "" -> Error Missing_command
  | "render" | "r" -> parse_render args
  | "help" ->
      let topic = if args = "" then None else Some args in
      Ok (Help topic)
  | "version" -> Ok Version
  | "source" -> Ok Source
  | other -> Ok (Unknown other)
