type t =
  { channels : in_channel * out_channel
  ; mutable closed : bool
  }

let protect action =
  try Ok (action ()) with
  | Sys_error message -> Error message
  | Unix.Unix_error (error, operation, _) ->
    Error (operation ^ ": " ^ Unix.error_message error)
;;

let start executable =
  protect (fun () ->
    { channels = Unix.open_process_args executable [| executable |]
    ; closed = false
    })
;;

let send engine command =
  if engine.closed
  then Error "Stockfish connection is closed."
  else (
    match Uci.command_to_string command with
    | Error error -> Error error
    | Ok line ->
      protect (fun () ->
        let _, output = engine.channels in
        output_string output (line ^ "\n");
        flush output))
;;

let receive engine =
  if engine.closed
  then Error "Stockfish connection is closed."
  else (
    let read () =
      try Ok (input_line (fst engine.channels)) with
      | End_of_file -> Error "Stockfish closed its output."
    in
    match protect read with
    | Error error | Ok (Error error) -> Error error
    | Ok (Ok line) -> Uci.response_of_string line)
;;

let close engine =
  if engine.closed
  then Error "Stockfish connection is closed."
  else (
    let sent = send engine Uci.Quit in
    engine.closed <- true;
    let status = protect (fun () -> Unix.close_process engine.channels) in
    match sent with
    | Error error -> Error error
    | Ok () -> status)
;;
