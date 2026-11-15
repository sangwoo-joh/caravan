module Reactor = Reactor
module Scheduler = Scheduler

type 'msg self = 'msg Actor.self
type 'msg behaviour = 'msg Actor.behaviour

module Address = Actor.Address

let spawn behaviour = Stdlib.Effect.perform (Actor.Spawn behaviour)
let send addr msg = Stdlib.Effect.perform (Actor.Send (addr, msg))

let receive self ?(matching = fun _ -> true) () =
  Stdlib.Effect.perform (Actor.Receive (self, matching))
;;

let address self = self
let await_readable fd = Stdlib.Effect.perform (Actor.Await_readable fd)
let sleep seconds = Stdlib.Effect.perform (Actor.Sleep seconds)
let stop addr = Stdlib.Effect.perform (Actor.Stop addr)

let run main =
  let module S = Scheduler.Make (Reactor.Select) in
  S.run main
;;
