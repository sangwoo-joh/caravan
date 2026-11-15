type 'msg self = 'msg Core.self
type 'msg behaviour = 'msg Core.behaviour

module Address = struct
  type 'msg t = 'msg Core.cell

  let equal a b = a.Core.identity == b.Core.identity
  let pp fmt a = Format.fprintf fmt "<actor:%d>" a.Core.id
end

let cast behaviour = Effect.perform (Core.Cast behaviour)
let send addr msg = Effect.perform (Core.Send (addr, msg))

let receive self ?(matching = fun _ -> true) () =
  Effect.perform (Core.Receive (self, matching))
;;

let address self = self
let await_readable fd = Effect.perform (Core.Await_readable fd)
let sleep seconds = Effect.perform (Core.Sleep seconds)
let stop addr = Effect.perform (Core.Stop addr)
