module L = Oif_lib.Line

let tests =
  [
    ( "make new line",
      `Quick,
      fun () ->
        let line = L.make ~id:1 ~text:"text" in
        Alcotest.(check int) "id" 1 line.id;
        Alcotest.(check string) "text" "text" line.text );
  ]
