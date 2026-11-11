type kind =
  | Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

type colour =
  | White
  | Black

type t =
  { id : int
  ; kind : kind
  ; colour : colour
  ; figure : string
  }

let next_piece_id = ref 0

let make_piece kind colour =
  let figure =
    match colour, kind with
    | White, Pawn -> "♙"
    | White, Knight -> "♘"
    | White, Bishop -> "♗"
    | White, Rook -> "♖"
    | White, Queen -> "♕"
    | White, King -> "♔"
    | Black, Pawn -> "♟"
    | Black, Knight -> "♞"
    | Black, Bishop -> "♝"
    | Black, Rook -> "♜"
    | Black, Queen -> "♛"
    | Black, King -> "♚"
  in
  incr next_piece_id;
  { id = !next_piece_id; kind; colour; figure }
;;
