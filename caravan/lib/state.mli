type t

val initial : unit -> t
val piece_at : t -> int -> Piece.t option
val turn : t -> Piece.colour
