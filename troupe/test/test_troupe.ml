let counter () =
  let result = ref 0 in
  Troupe.run (fun () ->
      let c =
        Troupe.cast (fun self ->
            let rec loop n =
              match Troupe.receive self () with
              | `Inc -> loop (n + 1)
              | `Get -> result := n
            in
            loop 0)
      in
      Troupe.send c `Inc;
      Troupe.send c `Inc;
      Troupe.send c `Inc;
      Troupe.send c `Get);
  Alcotest.(check int) "three increments observed in order" 3 !result

let selective_receive () =
  let order = ref [] in
  Troupe.run (fun () ->
      let a =
        Troupe.cast (fun self ->
            let x = Troupe.receive self ~matching:(fun s -> s = "B") () in
            order := x :: !order;
            let y = Troupe.receive self () in
            order := y :: !order)
      in
      Troupe.send a "A";
      Troupe.send a "B");
  Alcotest.(check (list string))
    "B handled before the earlier A, which stays queued" [ "B"; "A" ]
    (List.rev !order)

let request_reply () =
  let answer = ref "" in
  Troupe.run (fun () ->
      let server =
        Troupe.cast (fun self ->
            match Troupe.receive self () with
            | `Ping reply_to -> Troupe.send reply_to "pong")
      in
      let client =
        Troupe.cast (fun self -> answer := Troupe.receive self ())
      in
      Troupe.send server (`Ping client));
  Alcotest.(check string) "client received reply via its address" "pong" !answer

let wake_after_park () =
  (* [a] runs first with an empty mailbox and blocks; [b] wakes it later. *)
  let got = ref "" in
  Troupe.run (fun () ->
      let a = Troupe.cast (fun self -> got := Troupe.receive self ()) in
      let b =
        Troupe.cast (fun self ->
            let _ = Troupe.receive self () in
            Troupe.send a "woke")
      in
      Troupe.send b "go");
  Alcotest.(check string) "parked actor resumed on later send" "woke" !got

let backends : (string * ((unit -> unit) -> unit)) list =
  [
    ("select", Troupe.run);
    ( "poll",
      fun main ->
        let module S = Troupe.Scheduler.Make (Troupe.Reactor.Poll) in
        S.run main );
  ]

let await_readable run () =
  (* The reader parks on an empty pipe; the writer, running later, makes it
     readable and the scheduler wakes the reader out of the reactor. *)
  let got = ref "" in
  let r, w = Unix.pipe () in
  run (fun () ->
      let _ : unit Troupe.Address.t =
        Troupe.cast (fun (_ : unit Troupe.self) ->
            Troupe.await_readable r;
            let buf = Bytes.create 16 in
            let n = Unix.read r buf 0 16 in
            got := Bytes.sub_string buf 0 n)
      in
      let _ : unit Troupe.Address.t =
        Troupe.cast (fun (_ : unit Troupe.self) ->
            ignore (Unix.write_substring w "hi" 0 2))
      in
      ());
  Unix.close r;
  Unix.close w;
  Alcotest.(check string) "read what the writer sent" "hi" !got

let simultaneous_readers run () =
  let pipes = List.init 3 (fun _ -> Unix.pipe ()) in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun (r, w) -> Unix.close r; Unix.close w) pipes)
    (fun () ->
      let received = ref 0 in
      run (fun () ->
          List.iter
            (fun (r, _) ->
              ignore (Troupe.cast (fun _ ->
                  Troupe.await_readable r;
                  let buf = Bytes.create 1 in
                  received := !received + Unix.read r buf 0 1)))
            pipes;
          ignore (Troupe.cast (fun _ ->
              List.iter
                (fun (_, w) -> ignore (Unix.write_substring w "x" 0 1))
                pipes)));
      Alcotest.(check int) "every ready reader resumed" 3 !received)

let addresses_across_runs () =
  let old = ref None in
  Troupe.run (fun () -> old := Some (Troupe.cast (fun _ -> ())));
  Troupe.run (fun () ->
      let current = Troupe.cast (fun _ -> ()) in
      Alcotest.(check bool) "address equals itself" true
        (Troupe.Address.equal current current);
      Alcotest.(check bool) "different runs have distinct addresses" false
        (Troupe.Address.equal (Option.get !old) current))

let stop_old_addresses run () =
  let old_timer = ref None and old_reader = ref None in
  run (fun () ->
      old_timer := Some (Troupe.cast (fun _ -> ()));
      old_reader := Some (Troupe.cast (fun _ -> ())));
  let r, w = Unix.pipe () in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) (fun () ->
      let timer_fired = ref false and reader_fired = ref false in
      run (fun () ->
          ignore (Troupe.cast (fun _ ->
              Troupe.sleep 0.01;
              timer_fired := true));
          ignore (Troupe.cast (fun _ ->
              Troupe.await_readable r;
              reader_fired := true));
          Troupe.sleep 0.;
          Troupe.stop (Option.get !old_timer);
          Troupe.stop (Option.get !old_reader);
          ignore (Unix.write_substring w "x" 0 1));
      Alcotest.(check bool) "current timer survives" true !timer_fired;
      Alcotest.(check bool) "current reader survives" true !reader_fired)

let waits_during_message_traffic run () =
  let r, w = Unix.pipe () in
  Fun.protect ~finally:(fun () -> Unix.close r; Unix.close w) (fun () ->
      let timer_fired = ref false and reader_fired = ref false in
      let observed = ref (false, false) and exchanges = ref 0 in
      run (fun () ->
          ignore (Troupe.cast (fun _ ->
              Troupe.sleep 0.;
              timer_fired := true));
          ignore (Troupe.cast (fun _ ->
              Troupe.await_readable r;
              reader_fired := true));
          let peer = ref None in
          let rec exchange self other =
            Troupe.receive self ();
            incr exchanges;
            if !exchanges = 256 then
              observed := (!timer_fired, !reader_fired)
            else begin
              Troupe.send (other ()) ();
              exchange self other
            end
          in
          let a = Troupe.cast (fun self ->
              exchange self (fun () -> Option.get !peer)) in
          let b = Troupe.cast (fun self -> exchange self (fun () -> a)) in
          peer := Some b;
          Troupe.send a ();
          ignore (Unix.write_substring w "x" 0 1));
      Alcotest.(check (pair bool bool))
        "timer and reader run before message traffic ends" (true, true) !observed)

let timers_order run () =
  let log = ref [] in
  run (fun () ->
      let _ : unit Troupe.Address.t =
        Troupe.cast (fun (_ : unit Troupe.self) ->
            Troupe.sleep 0.03;
            log := "slow" :: !log)
      in
      let _ : unit Troupe.Address.t =
        Troupe.cast (fun (_ : unit Troupe.self) ->
            Troupe.sleep 0.005;
            log := "fast" :: !log)
      in
      ());
  Alcotest.(check (list string))
    "shorter sleep fires first" [ "fast"; "slow" ] (List.rev !log)

(* A cancelled actor must not resume from a timer, and its removal must let the
   scheduler settle rather than wait out the full sleep. *)
let stop_cancels_sleeping_actor () =
  let fired = ref false in
  Troupe.run (fun () ->
      let a = Troupe.cast (fun _ -> Troupe.sleep 2.0; fired := true) in
      Troupe.sleep 0.05;
      Troupe.stop a);
  Alcotest.(check bool) "sleeping actor cancelled before firing" false !fired

(* Stopping before the body has run at all drops the initial resume. *)
let stop_before_first_run () =
  let fired = ref false in
  Troupe.run (fun () ->
      let a = Troupe.cast (fun _ -> fired := true) in
      Troupe.stop a);
  Alcotest.(check bool) "actor stopped before running never ran" false !fired

let self_stop wait () =
  let module S = Troupe.Scheduler.Make (struct
    type t = unit
    let create () = ()
    let add_reader () _ = Alcotest.fail "stopped actor registered a reader"
    let remove_reader () _ = ()
    let wait () ~timeout:_ _ = Alcotest.fail "stopped actor kept scheduler waiting"
  end) in
  let reached_wait = ref false and resumed = ref false in
  S.run (fun () ->
      ignore (Troupe.cast (fun self ->
          Troupe.stop (Troupe.address self);
          reached_wait := true;
          wait ();
          resumed := true)));
  Alcotest.(check bool) "body reaches next suspension" true !reached_wait;
  Alcotest.(check bool) "body does not resume" false !resumed

let finished_mailbox mode () =
  let retained = ref None and messages = Weak.create 2 in
  let send_payload actor index =
    let payload = Bytes.make 1024 'x' in
    Weak.set messages index (Some payload);
    Troupe.send actor payload
  in
  (try
     Troupe.run (fun () ->
         let actor = Troupe.cast (fun _ -> if mode = `Raise then raise Exit) in
         retained := Some actor;
         send_payload actor 0;
         if mode = `Stop then Troupe.stop actor)
   with Exit -> ());
  Troupe.run (fun () -> send_payload (Option.get !retained) 1);
  Gc.full_major ();
  Alcotest.(check bool) "queued message released" false (Weak.check messages 0);
  Alcotest.(check bool) "later message discarded" false (Weak.check messages 1);
  ignore (Sys.opaque_identity !retained)

let () =
  Alcotest.run "troupe"
    [
      ( "actors",
        [
          Alcotest.test_case "counter (fifo order)" `Quick counter;
          Alcotest.test_case "selective receive" `Quick selective_receive;
          Alcotest.test_case "request/reply" `Quick request_reply;
          Alcotest.test_case "wake after park" `Quick wake_after_park;
          Alcotest.test_case "addresses across runs" `Quick addresses_across_runs;
          Alcotest.test_case "stop cancels sleeping actor" `Quick
            stop_cancels_sleeping_actor;
          Alcotest.test_case "stop before first run" `Quick stop_before_first_run;
          Alcotest.test_case "self stop before sleep" `Quick
            (self_stop (fun () -> Troupe.sleep 3600.));
          Alcotest.test_case "self stop before readable wait" `Quick
            (self_stop (fun () -> Troupe.await_readable Unix.stdin));
          Alcotest.test_case "stopped mailbox releases messages" `Quick
            (finished_mailbox `Stop);
          Alcotest.test_case "returned mailbox releases messages" `Quick
            (finished_mailbox `Return);
          Alcotest.test_case "failed mailbox releases messages" `Quick
            (finished_mailbox `Raise);
        ] );
      ( "reactor",
        List.concat_map
          (fun (name, run) ->
            [
              Alcotest.test_case
                (Printf.sprintf "await_readable (%s)" name)
                `Quick (await_readable run);
              Alcotest.test_case
                (Printf.sprintf "simultaneous readers (%s)" name)
                `Quick (simultaneous_readers run);
              Alcotest.test_case
                (Printf.sprintf "stop old addresses (%s)" name)
                `Quick (stop_old_addresses run);
              Alcotest.test_case
                (Printf.sprintf "waits during message traffic (%s)" name)
                `Quick (waits_during_message_traffic run);
              Alcotest.test_case
                (Printf.sprintf "timers fire in order (%s)" name)
                `Quick (timers_order run);
            ])
          backends );
    ]
