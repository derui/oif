open Oif_lib.Std

let tests =
  [
    ( "List.range should be able to get range inclusive",
      `Quick,
      fun () ->
        let range = List.range ~stop:`inclusive 1 5 in
        Alcotest.(check @@ list int) "inclusive" [ 1; 2; 3; 4; 5 ] range );
    ( "List.range should be able to get range exclusive",
      `Quick,
      fun () ->
        let range = List.range ~stop:`exclusive 1 5 in
        Alcotest.(check @@ list int) "inclusive" [ 1; 2; 3; 4 ] range );
    ( "List.range should use inclusive on default",
      `Quick,
      fun () ->
        let range = List.range 1 5 in
        Alcotest.(check @@ list int) "inclusive" [ 1; 2; 3; 4; 5 ] range );
    ( "List.range should return empty if end is larger than start",
      `Quick,
      fun () ->
        let range = List.range 1 0 in
        Alcotest.(check @@ list int) "empty" [] range );
    ( "List.range should return empty if start equals end and stop is exclusive",
      `Quick,
      fun () ->
        let range = List.range ~stop:`exclusive 1 1 in
        Alcotest.(check @@ list int) "empty" [] range );
    ( "List.range should return one element if start equals end and stop is inclusive",
      `Quick,
      fun () ->
        let range = List.range 1 1 in
        Alcotest.(check @@ list int) "one element" [ 1 ] range );
  ]
