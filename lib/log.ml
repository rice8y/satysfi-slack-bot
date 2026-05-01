type level = Quiet | Info | Debug

let level =
  let raw =
    (match Sys.getenv_opt "SATYSFI_BOT_LOG" with
    | Some value -> Some value
    | None -> Sys.getenv_opt "SATYSFI_BOT_LOG_LEVEL")
    |> Option.value ~default:"info"
    |> String.lowercase_ascii
  in
  match raw with
  | "0" | "false" | "off" | "quiet" | "error" -> Quiet
  | "debug" | "trace" | "verbose" | "1" | "true" | "on" -> Debug
  | _ -> Info

let enabled = function
  | Quiet -> false
  | Info -> level = Info || level = Debug
  | Debug -> level = Debug

let log level fmt =
  if enabled level then Printf.ksprintf (Printf.eprintf "%s\n%!") fmt
  else Printf.ksprintf ignore fmt

let info fmt = log Info fmt
let debug fmt = log Debug fmt
let error fmt = Printf.ksprintf (Printf.eprintf "%s\n%!") fmt
