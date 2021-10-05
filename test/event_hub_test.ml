open Oif_lib
module O = Oif
open CamomileLibraryDefault.Camomile

let tests =
  [
    ( "should be able to dispatch registered observers",
      `Quick,
      fun () ->
        let hub : string O.Event_hub.t = O.Event_hub.make () in
        let called = ref 0 in
        let observer v =
          Alcotest.(check string) "event" "event" v;
          incr called
        in
        O.Event_hub.add_observer observer hub |> ignore;
        O.Event_hub.dispatch "event" hub;
        Alcotest.(check int) "called" 1 !called );
    ( "should be able to delete observer from hub",
      `Quick,
      fun () ->
        let hub : string O.Event_hub.t = O.Event_hub.make () in
        let called = ref 0 in
        let observer v = incr called in
        let observer2 v = called := !called + 9 in
        let deleter = O.Event_hub.add_observer observer hub in
        O.Event_hub.add_observer observer2 hub |> ignore;
        deleter ();
        O.Event_hub.dispatch "event" hub;
        Alcotest.(check int) "called" 9 !called );
  ]
