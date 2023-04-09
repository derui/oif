module M = Oif_lib.Matcher
module V = Oif_lib.Vector
module C = Oif_lib.Candidate
module MR = Oif_lib.Match_result

let tests =
  let candidates () =
    let array = V.empty () in
    V.push ~value:(C.make ~id:1 ~text:"text") array;
    V.push ~value:(C.make ~id:2 ~text:"foo") array;
    V.push ~value:(C.make ~id:3 ~text:"bar") array;
    V.push ~value:(C.make ~id:4 ~text:"space data") array;
    array
  in
  let module Filter : Oif_lib.Filter.S = struct
    let unique_name = "test"

    let filter ~candidate ~query = Oif_lib.Match_result.make ~matched:[ (1, C.id candidate) ]
  end in
  let module Even_match_filter : Oif_lib.Filter.S = struct
    let unique_name = "test"

    let filter ~candidate ~query =
      if C.id candidate mod 2 = 0 then Oif_lib.Match_result.make ~matched:[ (1, C.id candidate) ]
      else Oif_lib.Match_result.empty
  end in
  [
    ( "should be able to apply filter to candidates",
      `Quick,
      fun () ->
        let candidates = ref @@ candidates () in
        let t = M.make ~candidates in
        M.apply_filter ~filter:(module Filter) ~query:"text" t;
        let actual = M.match_results t |> V.to_array |> Array.to_list |> List.map (fun v -> MR.matched v) in
        Alcotest.(check @@ list @@ list @@ pair int int)
          "queries"
          [ [ (1, 1) ]; [ (1, 2) ]; [ (1, 3) ]; [ (1, 4) ] ]
          actual );
    ( "should use referenced candidates",
      `Quick,
      fun () ->
        let candidates = ref @@ candidates () in
        V.push ~value:(C.make ~id:5 ~text:"append") !candidates;
        let t = M.make ~candidates in
        M.apply_filter ~filter:(module Filter) ~query:"text" t;
        let actual = M.match_results t |> V.to_array |> Array.to_list |> List.map (fun v -> MR.matched v) in
        Alcotest.(check @@ list @@ list @@ pair int int)
          "queries"
          [ [ (1, 1) ]; [ (1, 2) ]; [ (1, 3) ]; [ (1, 4) ]; [ (1, 5) ] ]
          actual );
    ( "should return only matched indices",
      `Quick,
      fun () ->
        let candidates = ref @@ candidates () in
        V.push ~value:(C.make ~id:5 ~text:"append") !candidates;
        let t = M.make ~candidates in
        M.apply_filter ~filter:(module Even_match_filter) ~query:"text" t;
        let actual = M.matched_indices t |> V.to_array |> Array.to_list in
        Alcotest.(check @@ list int) "queries" [ 1; 3 ] actual );
  ]
  |> List.map (fun (name, level, f) -> Alcotest_lwt.test_case_sync name level f)
