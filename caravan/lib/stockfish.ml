type t =
  { pid : int
  ; input : Unix.file_descr
  ; output : out_channel
  ; mutable pending : string
  ; mutable closed : bool
  }

type search_result =
  { state : State.t
  ; bestmove : State.move option
  ; info : string list
  }

let protect action =
  try Ok (action ()) with
  | Sys_error message -> Error message
  | Unix.Unix_error (error, operation, _) ->
    Error (operation ^ ": " ^ Unix.error_message error)
;;

let without_sigpipe action =
  let previous = Sys.signal Sys.sigpipe Sys.Signal_ignore in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigpipe previous) action
;;

let start executable =
  protect (fun () ->
    let input, child_output = Unix.pipe ~cloexec:true () in
    Fun.protect
      ~finally:(fun () -> Unix.close child_output)
      (fun () ->
         try
           let child_input, output = Unix.pipe ~cloexec:true () in
           Fun.protect
             ~finally:(fun () -> Unix.close child_input)
             (fun () ->
                try
                  let pid =
                    Unix.create_process
                      executable
                      [| executable |]
                      child_input
                      child_output
                      Unix.stderr
                  in
                  { pid
                  ; input
                  ; output = Unix.out_channel_of_descr output
                  ; pending = ""
                  ; closed = false
                  }
                with
                | exn ->
                  Unix.close output;
                  raise exn)
         with
         | exn ->
           Unix.close input;
           raise exn))
;;

let send engine command =
  if engine.closed
  then Error "Stockfish connection is closed."
  else (
    match Uci.command_to_string command with
    | Error error -> Error error
    | Ok line ->
      protect (fun () ->
        without_sigpipe (fun () ->
          output_string engine.output (line ^ "\n");
          flush engine.output)))
;;

let receive ?(timeout = 10.) engine =
  if engine.closed
  then Error "Stockfish connection is closed."
  else if (not (Float.is_finite timeout)) || timeout <= 0.
  then Error "Timeout must be positive and finite."
  else (
    let deadline = Unix.gettimeofday () +. timeout in
    let rec read () =
      match String.index_opt engine.pending '\n' with
      | Some index ->
        let line = String.sub engine.pending 0 index in
        engine.pending
        <- String.sub
             engine.pending
             (index + 1)
             (String.length engine.pending - index - 1);
        Uci.response_of_string line
      | None ->
        let remaining = deadline -. Unix.gettimeofday () in
        if remaining <= 0.
        then Error "Timed out waiting for Stockfish."
        else (
          let readable, _, _ = Unix.select [ engine.input ] [] [] remaining in
          if readable = []
          then Error "Timed out waiting for Stockfish."
          else (
            let bytes = Bytes.create 4096 in
            let count = Unix.read engine.input bytes 0 4096 in
            if count = 0
            then Error "Stockfish closed its output."
            else (
              engine.pending <- engine.pending ^ Bytes.sub_string bytes 0 count;
              read ())))
    in
    match protect read with
    | Ok result -> result
    | Error error -> Error error)
;;

let close engine =
  if engine.closed
  then Error "Stockfish connection is closed."
  else (
    ignore (send engine Uci.Quit);
    engine.closed <- true;
    without_sigpipe (fun () -> close_out_noerr engine.output);
    protect (fun () ->
      Unix.close engine.input;
      let deadline = Unix.gettimeofday () +. 1. in
      let rec wait () =
        match Unix.waitpid [ Unix.WNOHANG ] engine.pid with
        | 0, _ when Unix.gettimeofday () >= deadline ->
          Unix.kill engine.pid Sys.sigkill;
          snd (Unix.waitpid [] engine.pid)
        | 0, _ ->
          ignore (Unix.select [] [] [] 0.01);
          wait ()
        | _, status -> status
      in
      wait ()))
;;

let play_input ?(movetime = 1000) ~executable state input =
  let ( let* ) = Result.bind in
  let* _ = Uci.command_to_string (Uci.Go_movetime movetime) in
  let* state = Uci.prepare_move state input in
  match Logic.status state with
  | Checkmate _ | Draw _ -> Ok { state; bestmove = None; info = [] }
  | Playing | Check ->
    let* engine = start executable in
    let exchange () =
      let wait expected =
        let deadline = Unix.gettimeofday () +. 10. in
        let rec loop () =
          let* response =
            receive
              ~timeout:(max 0.001 (deadline -. Unix.gettimeofday ()))
              engine
          in
          if response = expected
          then Ok ()
          else if Unix.gettimeofday () >= deadline
          then Error "Stockfish handshake timed out."
          else loop ()
        in
        loop ()
      in
      let* () = send engine Uci.Uci in
      let* () = wait Uci.Uciok in
      let* () = send engine Uci.Isready in
      let* () = wait Uci.Readyok in
      let* () = send engine (Uci.Position_state state) in
      let* () = send engine (Uci.Go_movetime movetime) in
      let deadline =
        Unix.gettimeofday () +. (float_of_int movetime /. 1000.) +. 10.
      in
      let rec search info =
        if Unix.gettimeofday () >= deadline
        then Error "Stockfish search timed out."
        else
          let* response =
            receive ~timeout:(deadline -. Unix.gettimeofday ()) engine
          in
          match response with
          | Uci.Bestmove (Some move) ->
            let* state = Logic.play state move in
            Ok { state; bestmove = Some move; info = List.rev info }
          | Uci.Bestmove None ->
            Error "Stockfish returned no move in a playable position."
          | Uci.Info line
            when String.starts_with ~prefix:"info string CRITICAL ERROR" line ->
            Error line
          | Uci.Info line -> search (line :: info)
          | _ -> search info
      in
      search []
    in
    let outcome =
      Fun.protect
        ~finally:(fun () -> if not engine.closed then ignore (close engine))
        (fun () ->
           let outcome = exchange () in
           let closed = close engine in
           match outcome, closed with
           | Error error, _ | _, Error error -> Error error
           | Ok value, Ok (Unix.WEXITED 0) -> Ok value
           | Ok _, Ok _ -> Error "Stockfish exited unsuccessfully.")
    in
    outcome
;;
