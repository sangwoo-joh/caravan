(** Caravan screens and keyboard navigation. *)

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
