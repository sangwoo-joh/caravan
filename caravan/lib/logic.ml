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

let opposite = function
  | Piece.White -> Piece.Black
  | Piece.Black -> Piece.White
;;

let direction = function
  | Piece.White -> -1
  | Piece.Black -> 1
;;

let sign n = compare n 0

let clear_path piece_at source target =
  let step =
    (sign ((target / 8) - (source / 8)) * 8)
    + sign ((target mod 8) - (source mod 8))
  in
  let rec clear square =
    square = target || (piece_at square = None && clear (square + step))
  in
  clear (source + step)
;;

let attacks piece_at source target (piece : Piece.t) =
  let dx = abs ((target mod 8) - (source mod 8)) in
  let dy = (target / 8) - (source / 8) in
  source <> target
  &&
  match piece.kind with
  | Pawn -> dx = 1 && dy = direction piece.colour
  | Knight -> (dx = 1 && abs dy = 2) || (dx = 2 && abs dy = 1)
  | King -> dx <= 1 && abs dy <= 1
  | Bishop -> dx = abs dy && clear_path piece_at source target
  | Rook -> (dx = 0 || dy = 0) && clear_path piece_at source target
  | Queen ->
    (dx = abs dy || dx = 0 || dy = 0) && clear_path piece_at source target
;;

let attacked piece_at colour target =
  let rec loop source =
    if source = 64
    then false
    else (
      match piece_at source with
      | Some piece
        when piece.Piece.colour = colour && attacks piece_at source target piece
        -> true
      | _ -> loop (source + 1))
  in
  loop 0
;;

let in_check state colour =
  let rec find square =
    if square = 64
    then true
    else (
      match State.piece_at state square with
      | Some piece when piece.Piece.kind = King && piece.colour = colour ->
        attacked (State.piece_at state) (opposite colour) square
      | _ -> find (square + 1))
  in
  find 0
;;

let castle_allowed state (move : State.move) (piece : Piece.t) =
  let source = move.from_square
  and target = move.to_square in
  let home = if piece.colour = Piece.White then 60 else 4 in
  let side = if target > source then State.Kingside else State.Queenside in
  let step = if side = State.Kingside then 1 else -1 in
  let rook_square = home + if side = State.Kingside then 3 else -4 in
  let rook_present =
    match State.piece_at state rook_square with
    | Some rook -> rook.kind = Piece.Rook && rook.colour = piece.colour
    | None -> false
  in
  let without_king square =
    if square = source then None else State.piece_at state square
  in
  source = home
  && target = home + (2 * step)
  && State.can_castle state piece.colour side
  && rook_present
  && clear_path (State.piece_at state) home rook_square
  && (not (in_check state piece.colour))
  && not (attacked without_king (opposite piece.colour) (source + step))
;;

let pawn_allowed state (move : State.move) (piece : Piece.t) =
  let source = move.from_square
  and target = move.to_square in
  let step = direction piece.colour in
  let dx = abs ((target mod 8) - (source mod 8)) in
  let dy = (target / 8) - (source / 8) in
  let occupied = Option.is_some (State.piece_at state target) in
  if dx = 0
  then
    (not occupied)
    && (dy = step
        || (dy = 2 * step
            && (source / 8 = if piece.colour = Piece.White then 6 else 1)
            && State.piece_at state (source + (8 * step)) = None))
  else if dx = 1 && dy = step
  then
    occupied
    || (State.en_passant state = Some target
        &&
        match State.piece_at state (target - (8 * step)) with
        | Some pawn -> pawn.kind = Piece.Pawn && pawn.colour <> piece.colour
        | None -> false)
  else false
;;

let apply_unchecked state (move : State.move) (piece : Piece.t) =
  let source = move.from_square
  and target = move.to_square in
  let en_passant_capture =
    piece.kind = Piece.Pawn
    && source mod 8 <> target mod 8
    && State.piece_at state target = None
  in
  let captured =
    if en_passant_capture then target - (8 * direction piece.colour) else target
  in
  let placed =
    match move.promotion with
    | None -> piece
    | Some kind -> Piece.with_kind piece kind
  in
  let changes = [ source, None; captured, None; target, Some placed ] in
  let changes =
    if piece.kind = Piece.King && abs (target - source) = 2
    then (
      let rook_source, rook_target =
        if target > source
        then source + 3, source + 1
        else source - 4, source - 1
      in
      changes
      @ [ rook_source, None; rook_target, State.piece_at state rook_source ])
    else changes
  in
  let castling_rights =
    List.filter
      (fun (colour, side) ->
         let home = if colour = Piece.White then 60 else 4 in
         let rook = home + if side = State.Kingside then 3 else -4 in
         (not (piece.kind = Piece.King && piece.colour = colour))
         && source <> rook
         && target <> rook)
      (State.castling_rights state)
  in
  let en_passant =
    if piece.kind = Piece.Pawn && abs (target - source) = 16
    then Some ((source + target) / 2)
    else None
  in
  let halfmove_clock =
    if piece.kind = Piece.Pawn || State.piece_at state captured <> None
    then 0
    else State.halfmove_clock state + 1
  in
  State.advance_unchecked
    state
    move
    ~changes
    ~castling_rights
    ~en_passant
    ~halfmove_clock
;;

let checked_position state (move : State.move) =
  match State.piece_at state move.from_square with
  | None -> Error "There is no piece on the starting square."
  | Some piece when piece.colour <> State.turn state ->
    Error "It is the other colour's turn."
  | Some piece ->
    let target = State.piece_at state move.to_square in
    let forbidden =
      match target with
      | Some other -> other.colour = piece.colour || other.kind = Piece.King
      | None -> false
    in
    let promotes =
      piece.kind = Piece.Pawn
      && (move.to_square / 8 = 0 || move.to_square / 8 = 7)
    in
    if forbidden
    then Error "Cannot capture a friendly piece or a king."
    else if promotes <> Option.is_some move.promotion
    then Error "Invalid or missing promotion."
    else (
      let allowed =
        match piece.kind with
        | Pawn -> pawn_allowed state move piece
        | King when abs (move.to_square - move.from_square) = 2 ->
          castle_allowed state move piece
        | _ ->
          attacks (State.piece_at state) move.from_square move.to_square piece
      in
      if not allowed
      then Error "The piece cannot move to that square."
      else (
        let next = apply_unchecked state move piece in
        if in_check next piece.colour
        then Error "The move leaves your king in check."
        else Ok next))
;;

let validate_move state move =
  Result.map (fun _ -> ()) (checked_position state move)
;;

let legal_moves state =
  let moves = ref [] in
  for source = 0 to 63 do
    match State.piece_at state source with
    | Some piece when piece.colour = State.turn state ->
      for target = 0 to 63 do
        let promotions =
          if piece.kind = Piece.Pawn && (target / 8 = 0 || target / 8 = 7)
          then
            [ Some Piece.Queen
            ; Some Piece.Rook
            ; Some Piece.Bishop
            ; Some Piece.Knight
            ]
          else [ None ]
        in
        List.iter
          (fun promotion ->
             match State.move ?promotion source target with
             | Ok move when Result.is_ok (validate_move state move) ->
               moves := move :: !moves
             | _ -> ())
          promotions
      done
    | _ -> ()
  done;
  List.rev !moves
;;

let insufficient_material state =
  let pieces = ref [] in
  for square = 0 to 63 do
    match State.piece_at state square with
    | Some piece when piece.kind <> Piece.King ->
      pieces := (square, piece.kind) :: !pieces
    | _ -> ()
  done;
  match !pieces with
  | [] | [ (_, Piece.Bishop) ] | [ (_, Piece.Knight) ] -> true
  | (square, Piece.Bishop) :: rest ->
    let colour square = ((square / 8) + (square mod 8)) mod 2 in
    List.for_all
      (fun (other, kind) -> kind = Piece.Bishop && colour square = colour other)
      rest
  | _ -> false
;;

let effective_en_passant state =
  match State.en_passant state with
  | None -> None
  | Some target ->
    let source_row = (target / 8) - direction (State.turn state) in
    let available file =
      if file < 0 || file > 7
      then false
      else (
        let source = (source_row * 8) + file in
        match State.piece_at state source, State.move source target with
        | Some piece, Ok move when piece.kind = Piece.Pawn ->
          Result.is_ok (validate_move state move)
        | _ -> false)
    in
    if available ((target mod 8) - 1) || available ((target mod 8) + 1)
    then Some target
    else None
;;

let repetition_count state =
  let ep = effective_en_passant state in
  let same_position other =
    let rec board_equal square =
      if square = 64
      then true
      else (
        let identity =
          Option.map (fun (piece : Piece.t) -> piece.kind, piece.colour)
        in
        identity (State.piece_at state square)
        = identity (State.piece_at other square)
        && board_equal (square + 1))
    in
    State.turn state = State.turn other
    && List.for_all
         (fun (colour, side) ->
            State.can_castle state colour side
            = State.can_castle other colour side)
         [ Piece.White, State.Kingside
         ; White, Queenside
         ; Black, Kingside
         ; Black, Queenside
         ]
    && board_equal 0
    && ep = effective_en_passant other
  in
  let rec count state total =
    let total = total + if same_position state then 1 else 0 in
    match State.previous state with
    | None -> total
    | Some previous -> count previous total
  in
  count state 0
;;

let status state =
  let check = in_check state (State.turn state) in
  if legal_moves state = []
  then if check then Checkmate (opposite (State.turn state)) else Draw Stalemate
  else if insufficient_material state
  then Draw Insufficient_material
  else if State.halfmove_clock state >= 150
  then Draw Seventy_five_moves
  else if repetition_count state >= 5
  then Draw Fivefold_repetition
  else if check
  then Check
  else Playing
;;

let play state move =
  match status state with
  | Checkmate _ | Draw _ -> Error "The game has ended."
  | Playing | Check -> checked_position state move
;;

let claimable_draws state =
  match status state with
  | Checkmate _ | Draw _ -> []
  | Playing | Check ->
    (if repetition_count state >= 3 then [ Threefold_repetition ] else [])
    @ if State.halfmove_clock state >= 100 then [ Fifty_moves ] else []
;;

let claimable_draws_after state move =
  Result.map claimable_draws (play state move)
;;
