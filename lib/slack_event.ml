type file = {
  id : string option;
  name : string option;
  url_private_download : string option;
}

type event = {
  kind : string;
  user : string option;
  text : string option;
  ts : string option;
  channel : string option;
  subtype : string option;
  bot_id : string option;
  files : file list;
}

type body =
  | Url_verification of string
  | Event_callback of {
      team_id : string option;
      event_id : string;
      event : event;
    }
  | Unknown

let member name json = Yojson.Safe.Util.member name json
let to_string_option json = try Yojson.Safe.Util.to_string_option json with _ -> None

let parse_file json =
  let url_private_download =
    match member "url_private_download" json |> to_string_option with
    | Some url -> Some url
    | None -> member "url_private" json |> to_string_option
  in
  {
    id = member "id" json |> to_string_option;
    name = member "name" json |> to_string_option;
    url_private_download;
  }

let parse_files json =
  match member "files" json with
  | `List files -> List.map parse_file files
  | _ -> []

let parse_event json =
  {
    kind = (member "type" json |> Yojson.Safe.Util.to_string);
    user = member "user" json |> to_string_option;
    text = member "text" json |> to_string_option;
    ts = member "ts" json |> to_string_option;
    channel = member "channel" json |> to_string_option;
    subtype = member "subtype" json |> to_string_option;
    bot_id = member "bot_id" json |> to_string_option;
    files = parse_files json;
  }

let parse raw =
  let json = Yojson.Safe.from_string raw in
  match member "type" json |> Yojson.Safe.Util.to_string_option with
  | Some "url_verification" ->
      Url_verification (member "challenge" json |> Yojson.Safe.Util.to_string)
  | Some "event_callback" ->
      Event_callback
        {
          team_id = member "team_id" json |> to_string_option;
          event_id = member "event_id" json |> Yojson.Safe.Util.to_string;
          event = member "event" json |> parse_event;
        }
  | _ -> Unknown
