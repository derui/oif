open Oif_lib
module O = Oif
open CamomileLibraryDefault.Camomile

let tests =
  let get_key ?(control = false) ?(meta = false) ?(shift = false) code = { LTerm_key.code; shift; control; meta } in
  [
    ( "should be able to convert key event to json and from json",
      `Quick,
      fun () ->
        let key = get_key LTerm_key.End in
        let event = O.Events.make_key_event ~key ~timestamp:(Timestamp.of_int64 1L) in
        let actual = O.Events.(to_json event |> of_json |> Option.get) in
        Alcotest.(check @@ of_pp Fmt.nop) "same" actual event );
    ( "should be able to convert key event with modifiers",
      `Quick,
      fun () ->
        let key = get_key ~control:true ~shift:true LTerm_key.(Char (UChar.of_char 'b')) in
        let event = O.Events.make_key_event ~key ~timestamp:(Timestamp.of_int64 1L) in
        let actual = O.Events.(to_json event |> of_json |> Option.get) in
        Alcotest.(check @@ of_pp Fmt.nop) "same" actual event );
  ]
