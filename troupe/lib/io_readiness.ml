type timeout = [ `Int of int | `Float of float | `Infinite | `Default ]

module type S = sig
  type t

  val create : unit -> t
  val watch_readable_fd : t -> Unix.file_descr -> unit
  val unwatch_readable_fd : t -> Unix.file_descr -> unit
  val wait : t -> ?timeout:timeout -> (Unix.file_descr -> unit) -> unit
end

let timeout_seconds = function
  | `Int s -> Some (float_of_int (max 0 s))
  | `Float s -> Some (Float.max 0. s)
  | `Infinite -> None
  | `Default -> Some 600.
;;

module Select : S = struct
  type t = { mutable readers : Unix.file_descr list }

  let create () = { readers = [] }

  let watch_readable_fd t fd =
    if not (List.mem fd t.readers) then t.readers <- fd :: t.readers
  ;;

  let unwatch_readable_fd t fd = t.readers <- List.filter (fun x -> x <> fd) t.readers

  let wait t ?(timeout = `Default) f =
    let tmo =
      match timeout_seconds timeout with
      | None -> -1.0
      | Some s -> s
    in
    let ready, _, _ = Unix.select t.readers [] [] tmo in
    List.iter f ready
  ;;
end

module Poll : S = struct
  module P = Iomux.Poll

  (* Active readers occupy slots [0 .. count - 1] contiguously; [index] maps a
     descriptor to its slot so [unwatch_readable_fd] can compact by moving the last
     slot into the freed one. *)
  type t =
    { poll : P.t
    ; index : (Unix.file_descr, int) Hashtbl.t
    ; mutable count : int
    }

  let create () = { poll = P.create (); index = Hashtbl.create 8; count = 0 }

  let watch_readable_fd t fd =
    if not (Hashtbl.mem t.index fd)
    then (
      P.set_index t.poll t.count fd P.Flags.pollin;
      Hashtbl.replace t.index fd t.count;
      t.count <- t.count + 1)
  ;;

  let unwatch_readable_fd t fd =
    match Hashtbl.find_opt t.index fd with
    | None -> ()
    | Some i ->
      let last = t.count - 1 in
      if i <> last
      then (
        let moved = P.get_fd t.poll last in
        P.set_index t.poll i moved P.Flags.pollin;
        Hashtbl.replace t.index moved i);
      P.invalidate_index t.poll last;
      Hashtbl.remove t.index fd;
      t.count <- last
  ;;

  let wait t ?(timeout = `Default) f =
    let tmo : P.poll_timeout =
      match timeout_seconds timeout with
      | None -> P.Infinite
      | Some s ->
        let milliseconds = Float.ceil (s *. 1000.) in
        let limit =
          Float.min (Int32.to_float Int32.max_int) (float_of_int max_int)
        in
        P.Milliseconds (int_of_float (Float.min limit milliseconds))
    in
    let nready = P.poll t.poll t.count tmo in
    if nready > 0
    then (
      let ready = ref [] in
      P.iter_ready t.poll nready (fun _ fd _ -> ready := fd :: !ready);
      List.iter f (List.rev !ready))
  ;;
end
