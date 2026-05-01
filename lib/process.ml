type status = Exited of int | Signaled of int | Stopped of int

type result = { status : status; stdout : string; stderr : string }

let string_of_status = function
  | Exited n -> Printf.sprintf "exit status %d" n
  | Signaled n -> Printf.sprintf "signal %d" n
  | Stopped n -> Printf.sprintf "stopped by signal %d" n

let read_all fd =
  let buf = Buffer.create 4096 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> ()
    | n ->
        Buffer.add_subbytes buf bytes 0 n;
        loop ()
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ();
  Buffer.contents buf

let run_capture ?input ?cwd ?(timeout = 60.) command args =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  let argv = Array.of_list (command :: args) in
  let pid =
    match cwd with
    | None -> Unix.create_process command argv stdin_r stdout_w stderr_w
    | Some cwd ->
        let fork_pid = Unix.fork () in
        if fork_pid = 0 then (
          (try Unix.chdir cwd
           with exn ->
             Printf.eprintf "chdir failed: %s\n%!" (Printexc.to_string exn);
             exit 127);
          Unix.dup2 stdin_r Unix.stdin;
          Unix.dup2 stdout_w Unix.stdout;
          Unix.dup2 stderr_w Unix.stderr;
          Unix.close stdin_r;
          Unix.close stdin_w;
          Unix.close stdout_r;
          Unix.close stdout_w;
          Unix.close stderr_r;
          Unix.close stderr_w;
          (try Unix.execvp command argv
           with exn ->
             Printf.eprintf "exec failed: %s\n%!" (Printexc.to_string exn);
             exit 127));
        fork_pid
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  (match input with
  | None -> ()
  | Some data ->
      let len = String.length data in
      let rec write offset =
        if offset < len then
          let n = Unix.write_substring stdin_w data offset (len - offset) in
          write (offset + n)
      in
      write 0);
  Unix.close stdin_w;
  let out_value = ref None in
  let err_value = ref None in
  let out_thread = Thread.create (fun () -> out_value := Some (read_all stdout_r)) () in
  let err_thread = Thread.create (fun () -> err_value := Some (read_all stderr_r)) () in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec wait_loop () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () > deadline then (
          (try Unix.kill pid 9 with Unix.Unix_error _ -> ());
          let _, status = Unix.waitpid [] pid in
          status)
        else (
          Thread.delay 0.05;
          wait_loop ())
    | _, status -> status
  in
  let unix_status = wait_loop () in
  Thread.join out_thread;
  Thread.join err_thread;
  let stdout = Option.value !out_value ~default:"" in
  let stderr = Option.value !err_value ~default:"" in
  Unix.close stdout_r;
  Unix.close stderr_r;
  let status =
    match unix_status with
    | Unix.WEXITED n -> Exited n
    | Unix.WSIGNALED n -> Signaled n
    | Unix.WSTOPPED n -> Stopped n
  in
  { status; stdout; stderr }

let ensure_success ?what result =
  match result.status with
  | Exited 0 -> Ok result.stdout
  | status ->
      let label = Option.value what ~default:"process" in
      Error
        (Printf.sprintf "%s failed with %s\nstdout:\n%s\nstderr:\n%s" label
           (string_of_status status) result.stdout result.stderr)
