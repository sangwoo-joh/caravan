type move =
  { from_square : int
  ; to_square : int
  ; promotion : Piece.kind option
  }

type castle_side =
  | Kingside
  | Queenside

type action =
  | Think
  | Pick of int

type t =
  { board : Piece.t option array
  ; turn : Piece.colour
  ; user_colour : Piece.colour
  ; action : action
  ; castling_rights : (Piece.colour * castle_side) list
  ; en_passant : int option
  ; halfmove_clock : int
  ; fullmove_number : int
  ; previous : t option
  ; last_move : move option
  }

let move ?promotion from_square to_square =
  if from_square < 0 || from_square >= 64 || to_square < 0 || to_square >= 64
  then Error "Move squares must be between 0 and 63."
  else if from_square = to_square
  then Error "A move must change squares."
  else if promotion = Some Piece.Pawn || promotion = Some Piece.King
  then Error "Promote to a queen, rook, bishop, or knight."
  else Ok { from_square; to_square; promotion }
;;

let create
      ?(user_colour = Piece.White)
      ?(castling_rights = [])
      ?en_passant
      ?(halfmove_clock = 0)
      ?(fullmove_number = 1)
      ~turn
      board
  =
  let open Piece in
  let has square kind colour =
    match board.(square) with
    | Some piece -> piece.kind = kind && piece.colour = colour
    | None -> false
  in
  let kings colour =
    Array.fold_left
      (fun count -> function
         | Some piece when piece.kind = King && piece.colour = colour ->
           count + 1
         | _ -> count)
      0
      board
  in
  let rights_valid (colour, side) =
    let king = if colour = White then 60 else 4 in
    let rook = king + if side = Kingside then 3 else -4 in
    has king King colour && has rook Rook colour
  in
  let ep_valid = function
    | None -> true
    | Some square when square >= 0 && square < 64 ->
      let row, pawn, origin, opponent =
        if turn = White
        then 2, square + 8, square - 8, Black
        else 5, square - 8, square + 8, White
      in
      square / 8 = row
      && board.(square) = None
      && has pawn Pawn opponent
      && board.(origin) = None
      && halfmove_clock = 0
    | Some _ -> false
  in
  if Array.length board <> 64
  then Error "A board must contain 64 squares."
  else if kings White <> 1 || kings Black <> 1
  then Error "A board must contain one king of each colour."
  else if halfmove_clock < 0 || fullmove_number < 1
  then Error "Invalid move counters."
  else if
    List.exists
      (fun square -> has square Pawn White || has square Pawn Black)
      (List.init 8 Fun.id @ List.init 8 (fun n -> n + 56))
  then Error "Pawns cannot occupy the first or last rank."
  else if not (List.for_all rights_valid castling_rights)
  then
    Error "Castling rights require a king and rook on their starting squares."
  else if not (ep_valid en_passant)
  then Error "Invalid en passant target."
  else
    Ok
      { board = Array.copy board
      ; turn
      ; user_colour
      ; action = Think
      ; castling_rights = List.sort_uniq compare castling_rights
      ; en_passant
      ; halfmove_clock
      ; fullmove_number
      ; previous = None
      ; last_move = None
      }
;;

let initial ?(user_colour = Piece.White) () =
  let open Piece in
  let back_rank =
    [| Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook |]
  in
  let board =
    Array.init 64 (fun square ->
      let row = square / 8
      and col = square mod 8 in
      match row with
      | 0 -> Some (make_piece back_rank.(col) Black)
      | 1 -> Some (make_piece Pawn Black)
      | 6 -> Some (make_piece Pawn White)
      | 7 -> Some (make_piece back_rank.(col) White)
      | _ -> None)
  in
  { board
  ; turn = White
  ; user_colour
  ; action = Think
  ; castling_rights =
      [ White, Kingside; White, Queenside; Black, Kingside; Black, Queenside ]
  ; en_passant = None
  ; halfmove_clock = 0
  ; fullmove_number = 1
  ; previous = None
  ; last_move = None
  }
;;

let piece_at state square = state.board.(square)
let turn state = state.turn
let user_colour state = state.user_colour
let action state = state.action
let think state = { state with action = Think }

let pick state square =
  if square < 0 || square >= 64
  then Error "Square must be between 0 and 63."
  else (
    match state.board.(square) with
    | None -> Error "Select a square containing your piece."
    | Some piece when piece.colour <> state.user_colour ->
      Error "You can only select your own pieces."
    | Some _ -> Ok { state with action = Pick square })
;;

let castling_rights state = state.castling_rights
let can_castle state colour side = List.mem (colour, side) state.castling_rights
let en_passant state = state.en_passant
let halfmove_clock state = state.halfmove_clock
let fullmove_number state = state.fullmove_number
let previous state = state.previous

let moves state =
  let rec collect state acc =
    match state.previous, state.last_move with
    | Some previous, Some move -> collect previous (move :: acc)
    | _ -> acc
  in
  collect state []
;;

let advance_unchecked
      state
      move
      ~changes
      ~castling_rights
      ~en_passant
      ~halfmove_clock
  =
  let board = Array.copy state.board in
  List.iter (fun (square, piece) -> board.(square) <- piece) changes;
  { board
  ; turn = (if state.turn = Piece.White then Piece.Black else Piece.White)
  ; user_colour = state.user_colour
  ; action = Think
  ; castling_rights
  ; en_passant
  ; halfmove_clock
  ; fullmove_number =
      (state.fullmove_number + if state.turn = Piece.Black then 1 else 0)
  ; previous = Some state
  ; last_move = Some move
  }
;;
