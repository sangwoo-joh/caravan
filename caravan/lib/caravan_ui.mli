(** Caravan screens and keyboard navigation. *)

module Piece = Piece

(** The current screen, difficulty, and temporary session save. *)
type model

(** [initial] is the landing screen with normal difficulty selected. *)
val initial : model

(** [update key model] is the state and command after [key]. Ctrl+C requests
    termination. Moves are previews and saves last only for the current run. *)
val update
  :  Theaterminal.Event.key
  -> model
  -> model * Theaterminal.Event.key Theaterminal.Cmd.t

(** [view model] is the terminal frame for [model]. *)
val view : model -> string

(** [app] is the interactive Caravan screen prototype. *)
val app : (model, Theaterminal.Event.key) Theaterminal.app
