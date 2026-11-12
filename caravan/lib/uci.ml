type move = State.move

type command =
  | Uci
  | Isready
  | Ucinewgame
  | Position of move list
  | Position_state of State.t
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
  then (
    let square file rank =
      ((8 - (Char.code rank - Char.code '0')) * 8)
      + Char.code file
      - Char.code 'a'
    in
    let promotion =
      if n = 4
      then None
      else
        Some
          (match text.[4] with
           | 'q' -> Piece.Queen
           | 'r' -> Piece.Rook
           | 'b' -> Piece.Bishop
           | _ -> Piece.Knight)
    in
    State.move ?promotion (square text.[0] text.[1]) (square text.[2] text.[3]))
  else Error "Use a move such as e2e4 or e7e8q."
;;

let move_to_string (move : move) =
  let promotion =
    match move.promotion with
    | None -> ""
    | Some Piece.Queen -> "q"
    | Some Piece.Rook -> "r"
    | Some Piece.Bishop -> "b"
    | Some Piece.Knight -> "n"
    | Some (Piece.Pawn | Piece.King) -> invalid_arg "Uci.move_to_string"
  in
  square_name move.from_square ^ square_name move.to_square ^ promotion
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

let fen state =
  let piece_letter (piece : Piece.t) =
    let letter =
      match piece.kind with
      | Pawn -> 'p'
      | Knight -> 'n'
      | Bishop -> 'b'
      | Rook -> 'r'
      | Queen -> 'q'
      | King -> 'k'
    in
    if piece.colour = Piece.White then Char.uppercase_ascii letter else letter
  in
  let rank row =
    let buffer = Buffer.create 8
    and empty = ref 0 in
    let flush () =
      if !empty > 0 then Buffer.add_string buffer (string_of_int !empty);
      empty := 0
    in
    for file = 0 to 7 do
      match State.piece_at state ((row * 8) + file) with
      | None -> incr empty
      | Some piece ->
        flush ();
        Buffer.add_char buffer (piece_letter piece)
    done;
    flush ();
    Buffer.contents buffer
  in
  let rights =
    [ Piece.White, State.Kingside, "K"
    ; White, Queenside, "Q"
    ; Black, Kingside, "k"
    ; Black, Queenside, "q"
    ]
    |> List.filter_map (fun (colour, side, text) ->
      if State.can_castle state colour side then Some text else None)
    |> String.concat ""
  in
  Printf.sprintf
    "%s %s %s %s %d %d"
    (String.concat "/" (List.init 8 rank))
    (if State.turn state = Piece.White then "w" else "b")
    (if rights = "" then "-" else rights)
    (match State.en_passant state with
     | None -> "-"
     | Some square -> square_name square)
    (State.halfmove_clock state)
    (State.fullmove_number state)
;;

let position_command state =
  let rec root state =
    match State.previous state with
    | None -> state
    | Some previous -> root previous
  in
  let position = "position fen " ^ fen (root state) in
  match State.moves state with
  | [] -> position
  | moves ->
    position ^ " moves " ^ String.concat " " (List.map move_to_string moves)
;;

let prepare_move state input =
  match State.action state with
  | Pick _ ->
    Error "Finish placing your selected piece before entering a UCI move."
  | Think ->
    (match move_of_string (String.trim input) with
     | Error error -> Error error
     | Ok move ->
       (match State.pick state move.from_square with
        | Error error -> Error error
        | Ok _ -> Logic.play state move))
;;

let command_to_string = function
  | Uci -> Ok "uci"
  | Isready -> Ok "isready"
  | Ucinewgame -> Ok "ucinewgame"
  | Position_state state -> Ok (position_command state)
  | Position [] -> Ok "position startpos"
  | Position moves ->
    Ok
      ("position startpos moves "
       ^ String.concat " " (List.map move_to_string moves))
  | Go_movetime milliseconds when milliseconds > 0 ->
    Ok (Printf.sprintf "go movetime %d" milliseconds)
  | Go_movetime _ -> Error "Search time must be positive."
  | Stop -> Ok "stop"
  | Quit -> Ok "quit"
;;
