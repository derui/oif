module C = Oif_lib.Candidate

let tests =
  [
    ( "make new candidate",
      `Quick,
      fun () ->
        let line = C.make ~id:1 ~text:"text" in
        Alcotest.(check int) "id" 1 line.id;
        Alcotest.(check string) "text" "text" line.text );
  ]
