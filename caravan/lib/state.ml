type t =
  { board : Piece.t option array
  ; turn : Piece.colour
  }

let initial () =
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
  { board; turn = White }
;;

let piece_at state square = state.board.(square)
let turn state = state.turn
