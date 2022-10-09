open Oif_lib
module O = Oif
open CamomileLibraryDefault.Camomile

let tests =
  let make_timestamper_instance () =
    let module Time = struct
      let count = ref 1L

      let now () =
        let v = !count in
        count := Int64.add !count 2L;
        v
    end in
    let module TR = Timestamp.Make (Time) in
    (module (val TR.make ()) : Timestamp.Instance)
  in
  let key code = { LTerm_key.code; control = false; shift = false; meta = false } in

  [
    Alcotest_lwt.test_case_sync "should be able to dispatch registered observers" `Quick (fun () ->
        let module I = (val make_timestamper_instance ()) in
        let hub = O.Event_hub.make (module I) in
        let called = ref 0 in
        let observer v =
          let expected = O.Events.kind_of_key (key LTerm_key.Enter) in
          Alcotest.(check @@ of_pp Fmt.nop) "event" expected v.O.Events.kind;
          incr called
        in
        O.Event_hub.add_observer observer hub |> ignore;
        O.Event_hub.dispatch (O.Events.kind_of_key @@ key LTerm_key.Enter) hub;
        Alcotest.(check int) "called" 1 !called);
    Alcotest_lwt.test_case_sync "should be able to delete observer from hub" `Quick (fun () ->
        let module I = (val make_timestamper_instance ()) in
        let hub = O.Event_hub.make (module I) in
        let called = ref 0 in
        let observer v = incr called in
        let observer2 v = called := !called + 9 in
        let deleter = O.Event_hub.add_observer observer hub in
        O.Event_hub.add_observer observer2 hub |> ignore;
        O.Event_hub.call_deleter deleter;
        let kind = O.Events.kind_of_key @@ key LTerm_key.Enter in
        O.Event_hub.dispatch kind hub;
        Alcotest.(check int) "called" 9 !called);
  ]
