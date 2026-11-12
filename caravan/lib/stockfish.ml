type t =
  { pid : int
  ; input : Unix.file_descr
  ; output : out_channel
  ; mutable pending : string
  ; mutable exit_status : (Unix.process_status, string) result option
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
                  ; exit_status = None
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

let pause seconds = ignore (Unix.select [] [] [] seconds)

let receive ?(pause = pause) ?(timeout = 10.) engine =
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
          let readable, _, _ = Unix.select [ engine.input ] [] [] 0. in
          if readable = []
          then (
            pause (min remaining 0.01);
            read ())
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

let poll_exit engine =
  match engine.exit_status with
  | Some _ as status -> status
  | None ->
    let status =
      try
        match Unix.waitpid [ Unix.WNOHANG ] engine.pid with
        | 0, _ -> None
        | _, status -> Some (Ok status)
      with
      | Unix.Unix_error (Unix.EINTR, _, _) -> None
      | Unix.Unix_error (error, operation, _) ->
        Some (Error (operation ^ ": " ^ Unix.error_message error))
    in
    engine.exit_status <- status;
    status
;;

let rec wait ?(pause = pause) engine =
  match poll_exit engine with
  | Some status -> status
  | None ->
    pause 0.01;
    wait ~pause engine
;;

let terminate engine =
  if not engine.closed
  then (
    engine.closed <- true;
    without_sigpipe (fun () -> close_out_noerr engine.output);
    ignore (protect (fun () -> Unix.close engine.input));
    if poll_exit engine = None
    then ignore (protect (fun () -> Unix.kill engine.pid Sys.sigkill)))
;;

let close ?(pause = pause) engine =
  if engine.closed
  then Error "Stockfish connection is closed."
  else (
    ignore (send engine Uci.Quit);
    let deadline = Unix.gettimeofday () +. 1. in
    let rec wait () =
      match poll_exit engine with
      | Some status ->
        terminate engine;
        status
      | None ->
        if Unix.gettimeofday () >= deadline then terminate engine;
        pause 0.01;
        wait ()
    in
    wait ())
;;

let search ?(pause = pause) ?(skill = 5) ?(movetime = 1000) engine state =
  let ( let* ) = Result.bind in
  let* _ = Uci.command_to_string (Uci.Skill_level skill) in
  let* _ = Uci.command_to_string (Uci.Go_movetime movetime) in
  let wait expected =
    let deadline = Unix.gettimeofday () +. 10. in
    let rec loop () =
      if Unix.gettimeofday () >= deadline
      then Error "Stockfish handshake timed out."
      else
        let* response =
          receive ~pause ~timeout:(deadline -. Unix.gettimeofday ()) engine
        in
        if response = expected then Ok () else loop ()
    in
    loop ()
  in
  let* () = send engine Uci.Uci in
  let* () = wait Uci.Uciok in
  let* () = send engine (Uci.Limit_strength false) in
  let* () = send engine (Uci.Skill_level skill) in
  let* () = send engine Uci.Isready in
  let* () = wait Uci.Readyok in
  let* () = send engine (Uci.Position_state state) in
  let* () = send engine (Uci.Go_movetime movetime) in
  let deadline =
    Unix.gettimeofday () +. (float_of_int movetime /. 1000.) +. 10.
  in
  let rec collect info =
    if Unix.gettimeofday () >= deadline
    then Error "Stockfish search timed out."
    else
      let* response =
        receive ~pause ~timeout:(deadline -. Unix.gettimeofday ()) engine
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
      | Uci.Info line -> collect (line :: info)
      | _ -> collect info
  in
  collect []
;;

let analyse ?(skill = 5) ?(movetime = 1000) ~executable state =
  let ( let* ) = Result.bind in
  let* _ = Uci.command_to_string (Uci.Skill_level skill) in
  let* _ = Uci.command_to_string (Uci.Go_movetime movetime) in
  match Logic.status state with
  | Checkmate _ | Draw _ -> Ok { state; bestmove = None; info = [] }
  | Playing | Check ->
    let* engine = start executable in
    Fun.protect
      ~finally:(fun () ->
        if not engine.closed then terminate engine;
        ignore (wait engine))
      (fun () ->
         let outcome = search ~skill ~movetime engine state in
         let closed = close engine in
         match outcome, closed with
         | Error error, _ | _, Error error -> Error error
         | Ok value, Ok (Unix.WEXITED 0) -> Ok value
         | Ok _, Ok _ -> Error "Stockfish exited unsuccessfully.")
;;

let play_input ?(skill = 5) ?(movetime = 1000) ~executable state input =
  Result.bind
    (Uci.prepare_move state input)
    (analyse ~skill ~movetime ~executable)
;;
