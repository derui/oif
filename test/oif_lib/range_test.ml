module R = Oif_lib.Range

let tests =
  [
    ( "return empty list if empty",
      `Quick,
      fun () -> Alcotest.(check @@ list @@ pair int int) "empty list" [] @@ R.merge [] );
    ( "return single list if list have adjacent or overlapped range",
      `Quick,
      fun () ->
        let ranges = [ (1, 3); (0, 0); (0, 1) ] in
        Alcotest.(check @@ list @@ pair int int) "merged" [ (0, 3) ] @@ R.merge ranges );
    ( "return multi list if list have other ranges that are adjacent or overlapped independent",
      `Quick,
      fun () ->
        let ranges = [ (1, 3); (2, 5); (0, 0) ] in
        Alcotest.(check @@ list @@ pair int int) "merged" [ (0, 0); (1, 5) ] @@ R.merge ranges );
  ]
  |> List.map (fun (name, level, f) -> Alcotest_lwt.test_case_sync name level f)
