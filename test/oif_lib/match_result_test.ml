open Oif_lib.Std
module M = Oif_lib.Match_result

let tests =
  [
    ( "make empty result",
      `Quick,
      fun () ->
        let result = M.empty in
        Alcotest.(check @@ list @@ pair int int) "length" [] @@ M.matched result );
    ( "should return it is not matched if is_matched is given empty",
      `Quick,
      fun () ->
        let ret = M.empty in
        Alcotest.(check bool) "length" false @@ M.is_matched ret );
    ( "should return it is matched if least one matching pairs",
      `Quick,
      fun () ->
        let ret = M.make ~matched:[ (1, 2) ] in
        Alcotest.(check bool) "length" true @@ M.is_matched ret );
  ]
  |> List.map ~f:(fun (name, level, f) -> Alcotest_lwt.test_case_sync name level f)
