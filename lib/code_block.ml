let zero_width_joiner = "\226\128\141"

let sanitize raw =
  let marker = "```" in
  let marker_len = String.length marker in
  let buf = Buffer.create (String.length raw) in
  let rec loop start =
    match String.index_from_opt raw start '`' with
    | None -> Buffer.add_substring buf raw start (String.length raw - start)
    | Some i when i + marker_len <= String.length raw
                  && String.sub raw i marker_len = marker ->
        Buffer.add_substring buf raw start (i - start);
        Buffer.add_string buf "``";
        Buffer.add_string buf zero_width_joiner;
        Buffer.add_char buf '`';
        loop (i + marker_len)
    | Some i ->
        Buffer.add_substring buf raw start (i - start + 1);
        loop (i + 1)
  in
  loop 0;
  Buffer.contents buf

let wrap raw = Printf.sprintf "```\n%s\n```" (sanitize raw)

let truncate max_len raw =
  if String.length raw <= max_len then raw
  else String.sub raw 0 max_len ^ "\n... truncated ..."
