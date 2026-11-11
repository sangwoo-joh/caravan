type move = string

type command =
  | Uci
  | Isready
  | Ucinewgame
  | Position of move list
  | Go_movetime of int
  | Stop
  | Quit

type response =
  | Uciok
  | Readyok
  | Bestmove of move option
  | Info of string
  | Other of string

let square_name square =
  if square < 0 || square >= 64 then invalid_arg "Uci.square_name";
  Printf.sprintf
    "%c%d"
    (Char.chr (Char.code 'a' + (square mod 8)))
    (8 - (square / 8))
;;

let move_of_string text =
  let file c = c >= 'a' && c <= 'h' in
  let rank c = c >= '1' && c <= '8' in
  let n = String.length text in
  if
    (n = 4 || n = 5)
    && file text.[0]
    && rank text.[1]
    && file text.[2]
    && rank text.[3]
    && (n = 4 || String.contains "qrbn" text.[4])
  then Ok text
  else Error "Use a move such as e2e4 or e7e8q."
;;

let move_to_string move = move

let command_to_string = function
  | Uci -> Ok "uci"
  | Isready -> Ok "isready"
  | Ucinewgame -> Ok "ucinewgame"
  | Position [] -> Ok "position startpos"
  | Position moves -> Ok ("position startpos moves " ^ String.concat " " moves)
  | Go_movetime milliseconds when milliseconds > 0 ->
    Ok (Printf.sprintf "go movetime %d" milliseconds)
  | Go_movetime _ -> Error "Search time must be positive."
  | Stop -> Ok "stop"
  | Quit -> Ok "quit"
;;

let response_of_string line =
  let line = String.trim line in
  let words =
    String.split_on_char ' ' line |> List.filter (fun word -> word <> "")
  in
  let bestmove text =
    if text = "(none)" || text = "0000"
    then Ok (Bestmove None)
    else Result.map (fun move -> Bestmove (Some move)) (move_of_string text)
  in
  match words with
  | [ "uciok" ] -> Ok Uciok
  | [ "readyok" ] -> Ok Readyok
  | [ "bestmove"; move ] -> bestmove move
  | [ "bestmove"; move; "ponder"; ponder ] ->
    (match move_of_string ponder with
     | Error error -> Error error
     | Ok _ -> bestmove move)
  | "bestmove" :: _ -> Error "Malformed bestmove response."
  | "info" :: _ -> Ok (Info line)
  | _ -> Ok (Other line)
;;
