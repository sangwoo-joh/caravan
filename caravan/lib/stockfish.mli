type t

val start : string -> (t, string) result
val send : t -> Uci.command -> (unit, string) result
val receive : t -> (Uci.response, string) result
val close : t -> (Unix.process_status, string) result
