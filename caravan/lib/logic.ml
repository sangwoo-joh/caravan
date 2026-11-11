let material_value = function
  | Piece.Pawn -> 100
  | Piece.Knight -> 320
  | Piece.Bishop -> 330
  | Piece.Rook -> 500
  | Piece.Queen -> 900
  | Piece.King -> 0
;;

let material_balance state =
  let total = ref 0 in
  for square = 0 to 63 do
    match State.piece_at state square with
    | None -> ()
    | Some piece ->
      let value = material_value piece.kind in
      total
      := !total
         +
           (match piece.colour with
           | White -> value
           | Black -> -value)
  done;
  !total
;;
