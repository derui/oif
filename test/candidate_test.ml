module L = Oif_lib.Line
module C = Oif_lib.Candidate

let tests =
  [
    ( "make new candidate",
      `Quick,
      fun () ->
        let line = L.make ~id:1 ~text:"text" in
        let candidate = C.make line in
        Alcotest.(check int) "id" 1 @@ C.id candidate;
        Alcotest.(check string) "text" "text" @@ C.text candidate;
        Alcotest.(check @@ list @@ pair int int) "matched" [] @@ C.matched candidate );
    ( "should be able to get matched list if it is passed",
      `Quick,
      fun () ->
        let line = L.make ~id:1 ~text:"text" in
        let candidate = C.make ~matched:[ (1, 3) ] line in
        Alcotest.(check @@ list @@ pair int int) "matched" [ (1, 3) ] @@ C.matched candidate );
  ]
