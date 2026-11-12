type draw_reason =
  | Stalemate
  | Insufficient_material
  | Fivefold_repetition
  | Seventy_five_moves

type status =
  | Playing
  | Check
  | Checkmate of Piece.colour
  | Draw of draw_reason

type draw_claim =
  | Threefold_repetition
  | Fifty_moves

val material_value : Piece.kind -> int
val material_balance : State.t -> int
val in_check : State.t -> Piece.colour -> bool
val validate_move : State.t -> State.move -> (unit, string) result
val legal_moves : State.t -> State.move list
val status : State.t -> status
val play : State.t -> State.move -> (State.t, string) result
val claimable_draws : State.t -> draw_claim list

val claimable_draws_after
  :  State.t
  -> State.move
  -> (draw_claim list, string) result
