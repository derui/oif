open Oif_lib

let tests =
  [
    ( "make and get timestamp from int64",
      `Quick,
      fun () ->
        let data = Timestamp.of_int64 64L in
        let actual = Timestamp.to_int64 data in
        Alcotest.(check int64) "same timestamp" 64L actual );
    ( "get positive difference",
      `Quick,
      fun () ->
        let before = Timestamp.of_int64 64L in
        let after = Timestamp.of_int64 65L in
        let actual = Timestamp.difference ~before ~after |> Timestamp.to_int64 in
        Alcotest.(check int64) "same timestamp" 1L actual );
    ( "get negative difference",
      `Quick,
      fun () ->
        let before = Timestamp.of_int64 65L in
        let after = Timestamp.of_int64 64L in
        let actual = Timestamp.difference ~before ~after |> Timestamp.to_int64 in
        Alcotest.(check int64) "same timestamp" (-1L) actual );
  ]
