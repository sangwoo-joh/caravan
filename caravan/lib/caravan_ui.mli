(** Caravan screens and keyboard navigation. *)

type piece_kind = Pawn | Knight | Bishop | Rook | Queen | King
type piece_colour = White | Black
type piece = {
  id : int;
  kind : piece_kind;
  colour : piece_colour;
  figure : string;
}

val make_piece : piece_kind -> piece_colour -> piece
(** [make_piece kind colour] is a new piece of [kind] and [colour] with the
    corresponding Unicode chess figure. IDs start at 1 and increase with each
    call during the current process. Calls must be made sequentially. *)

type model
(** The current screen, difficulty, and temporary session save. *)

val initial : model
(** [initial] is the landing screen with normal difficulty selected. *)

val update : Theaterminal.Event.key -> model -> model * Theaterminal.Event.key Theaterminal.Cmd.t
(** [update key model] is the state and command after [key]. Ctrl+C requests
    termination. Moves are previews and saves last only for the current run. *)

val view : model -> string
(** [view model] is the terminal frame for [model]. *)

val app : (model, Theaterminal.Event.key) Theaterminal.app
(** [app] is the interactive Caravan screen prototype. *)
