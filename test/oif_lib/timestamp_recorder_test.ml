open Oif_lib
module T = Timestamp
module TR = Timestamp_recorder

let tests =
  [
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
        let recorder = TR.start (module Time) in
        let time = TR.make_stamp recorder |> T.to_int64 in
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
        let recorder = TR.start (module Time) in
        TR.make_stamp recorder |> ignore;
        TR.make_stamp recorder |> ignore;
        let time = TR.make_stamp recorder |> T.to_int64 in

        Alcotest.(check int64) "recorded" 6L time );
  ]
