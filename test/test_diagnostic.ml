open Satysfi_slack_bot

let sample =
  {|
 ---- ---- ---- ----
  target file: '/private/tmp/bad2.pdf'
  parsing '/private/tmp/bad2.saty' ...
! [Syntax Error at Parser] at "bad2.saty", line 5, characters 2-6:
! [Error] /private/tmp/no-such.saty: No such file or directory
|}

let () =
  match Diagnostics.parse_satysfi_output sample with
  | [ first; second ] ->
      assert (first.severity = Diagnostics.Error);
      assert (first.phase = "Syntax Error at Parser");
      assert (
        first.span
        = Some
            { Diagnostics.file = "bad2.saty"; line = 5; start_col = 2; end_col = 6 });
      assert (second.severity = Diagnostics.Error);
      assert (second.span = None);
      assert (
        second.message = "/private/tmp/no-such.saty: No such file or directory");
      let formatted = Diagnostics.format_diagnostics [ first; second ] in
      assert (String.contains formatted '5')
  | diagnostics ->
      failwith
        (Printf.sprintf "expected 2 diagnostics, got %d" (List.length diagnostics))
