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
    ( "get timestamp from start",
      `Quick,
      fun () ->
        let module Time = struct
          let count = ref 1L

          let now () =
            let v = !count in
            count := Int64.add !count 2L;
            v
        end in
        let module TR = Timestamp.Make (Time) in
        let module I = (val TR.make ()) in
        I.(start instance);
        let time = I.(timestamp instance) |> Timestamp.to_int64 in
        Alcotest.(check int64) "recorded" 2L time );
    ( "get monotonous increased timestamp from start",
      `Quick,
      fun () ->
        let module Time = struct
          let count = ref 1L

          let now () =
            let v = !count in
            count := Int64.add !count 2L;
            v
        end in
        let module TR = Timestamp.Make (Time) in
        let module TR = Timestamp.Make (Time) in
        let module I = (val TR.make ()) in
        I.(start instance);
        I.(timestamp instance) |> ignore;
        I.(timestamp instance) |> ignore;
        let time = I.(timestamp instance) |> Timestamp.to_int64 in

        Alcotest.(check int64) "recorded" 6L time );
  ]
