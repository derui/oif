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
    ( "List.range do not allow use minus value",
      `Quick,
      fun () ->
        Alcotest.(check @@ list int) "from" [] @@ List.range (-1) 0;
        Alcotest.(check @@ list int) "to" [] @@ List.range 0 (-1) );
    (* Seq version *)
    ( "Seq.range should be able to get range inclusive",
      `Quick,
      fun () ->
        let range = Seq.range ~stop:`inclusive 1 5 |> List.of_seq in
        Alcotest.(check @@ list int) "inclusive" [ 1; 2; 3; 4; 5 ] range );
    ( "Seq.range should be able to get range exclusive",
      `Quick,
      fun () ->
        let range = Seq.range ~stop:`exclusive 1 5 |> List.of_seq in
        Alcotest.(check @@ list int) "inclusive" [ 1; 2; 3; 4 ] range );
    ( "Seq.range should use inclusive on default",
      `Quick,
      fun () ->
        let range = Seq.range 1 5 |> List.of_seq in
        Alcotest.(check @@ list int) "inclusive" [ 1; 2; 3; 4; 5 ] range );
    ( "Seq.range should return empty if end is larger than start",
      `Quick,
      fun () ->
        let range = Seq.range 1 0 |> List.of_seq in
        Alcotest.(check @@ list int) "empty" [] range );
    ( "Seq.range should return empty if start equals end and stop is exclusive",
      `Quick,
      fun () ->
        let range = Seq.range ~stop:`exclusive 1 1 |> List.of_seq in
        Alcotest.(check @@ list int) "empty" [] range );
    ( "Seq.range should return one element if start equals end and stop is inclusive",
      `Quick,
      fun () ->
        let range = Seq.range 1 1 |> List.of_seq in
        Alcotest.(check @@ list int) "one element" [ 1 ] range );
    ( "Seq.range do not allow use minus value",
      `Quick,
      fun () ->
        Alcotest.(check @@ list int) "from" [] (Seq.range (-1) 0 |> List.of_seq);
        Alcotest.(check @@ list int) "to" [] (Seq.range 0 (-1) |> List.of_seq) );
  ]
  |> List.map ~f:(fun (name, level, f) -> Alcotest_lwt.test_case_sync name level f)
