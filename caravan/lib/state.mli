type move = private
  { from_square : int
  ; to_square : int
  ; promotion : Piece.kind option
  }

type castle_side =
  | Kingside
  | Queenside

type t

val move : ?promotion:Piece.kind -> int -> int -> (move, string) result
val initial : unit -> t

val create
  :  ?castling_rights:(Piece.colour * castle_side) list
  -> ?en_passant:int
  -> ?halfmove_clock:int
  -> ?fullmove_number:int
  -> turn:Piece.colour
  -> Piece.t option array
  -> (t, string) result

val piece_at : t -> int -> Piece.t option
val turn : t -> Piece.colour
val castling_rights : t -> (Piece.colour * castle_side) list
val can_castle : t -> Piece.colour -> castle_side -> bool
val en_passant : t -> int option
val halfmove_clock : t -> int
val fullmove_number : t -> int
val previous : t -> t option
val moves : t -> move list

val advance_unchecked
  :  t
  -> move
  -> changes:(int * Piece.t option) list
  -> castling_rights:(Piece.colour * castle_side) list
  -> en_passant:int option
  -> halfmove_clock:int
  -> t
