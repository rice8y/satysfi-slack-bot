type failure =
  | Missing_timestamp
  | Missing_signature
  | Unsupported_version of string
  | Invalid_timestamp of string
  | Timestamp_out_of_range
  | Signature_mismatch

let string_of_failure = function
  | Missing_timestamp -> "missing X-Slack-Request-Timestamp"
  | Missing_signature -> "missing X-Slack-Signature"
  | Unsupported_version version -> "unsupported Slack signature version: " ^ version
  | Invalid_timestamp timestamp -> "invalid Slack timestamp: " ^ timestamp
  | Timestamp_out_of_range -> "Slack timestamp outside allowed tolerance"
  | Signature_mismatch -> "Slack signature mismatch"

let signing_base ~version ~timestamp ~body = version ^ ":" ^ timestamp ^ ":" ^ body

let sign ~secret ~timestamp ~body =
  "v0=" ^ Sha256.hmac_sha256_hex ~key:secret ~data:(signing_base ~version:"v0" ~timestamp ~body)

let split_signature signature =
  match String.index_opt signature '=' with
  | None -> ("", signature)
  | Some index ->
      let version = String.sub signature 0 index in
      let digest =
        String.sub signature (index + 1) (String.length signature - index - 1)
      in
      (version, digest)

let verify ?(tolerance_seconds = 300) ~now ~secret ~timestamp ~signature ~body () =
  match (timestamp, signature) with
  | None, _ -> Error Missing_timestamp
  | _, None -> Error Missing_signature
  | Some timestamp, Some signature -> (
      let version, _ = split_signature signature in
      if version <> "v0" then Error (Unsupported_version version)
      else
        match int_of_string_opt timestamp with
        | None -> Error (Invalid_timestamp timestamp)
        | Some request_time ->
            if abs (now - request_time) > tolerance_seconds then
              Error Timestamp_out_of_range
            else
              let expected = sign ~secret ~timestamp ~body in
              if Sha256.equal_constant_time expected signature then Ok ()
              else Error Signature_mismatch)
