open Oif_lib
module O = Oif
open Camomile

let tests =
  let get_key ?(control = false) ?(meta = false) ?(shift = false) code = { LTerm_key.code; shift; control; meta } in
  [
    Alcotest_lwt.test_case_sync "should be able to convert key event to json and from json" `Quick (fun () ->
        let key = get_key LTerm_key.End in
        let kind = O.Events.kind_of_key key in
        let event = O.Events.make ~kind ~timestamp:(Timestamp.of_int64 1L) in
        let actual = O.Events.(to_json event |> of_json |> Option.get) in
        Alcotest.(check @@ of_pp Fmt.nop) "same" actual event);
    Alcotest_lwt.test_case_sync "should be able to convert key event with modifiers" `Quick (fun () ->
        let key = get_key ~control:true ~shift:true LTerm_key.(Char (Uchar.of_char 'b')) in
        let kind = O.Events.kind_of_key key in
        let event = O.Events.make ~kind ~timestamp:(Timestamp.of_int64 1L) in
        let actual = O.Events.(to_json event |> of_json |> Option.get) in
        Alcotest.(check @@ of_pp Fmt.nop) "same" actual event);
  ]
