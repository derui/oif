open Oif_lib
module M = New_matcher
module V = Vector
module C = Candidate
module MR = Match_result

let candidates () =
  let array = V.empty () in
  V.push ~value:(C.make ~id:1 ~text:"text") array;
  V.push ~value:(C.make ~id:2 ~text:"foo") array;
  V.push ~value:(C.make ~id:3 ~text:"bar") array;
  V.push ~value:(C.make ~id:4 ~text:"space data") array;
  array

module Filter : Filter.S = struct
  let unique_name = "test"

  let filter ~candidate ~query = Match_result.make ~matched:[ (1, C.id candidate) ]
end

module Even_match_filter : Oif_lib.Filter.S = struct
  let unique_name = "test"

  let filter ~candidate ~query =
    if C.id candidate mod 2 = 0 then Match_result.make ~matched:[ (1, C.id candidate) ] else Match_result.empty
end

let test1 =
  ( "should be able to apply filter to candidates",
    `Quick,
    fun _ () ->
      let t = M.make () in
      V.to_seq @@ candidates ()
      |> Lwt_seq.of_seq
      |> Lwt_seq.iter_s (fun v -> M.add_candidate ~candidate:v ~filter:(module Filter) t);%lwt
      let%lwt () = M.apply_filter ~filter:(module Filter) ~query:"text" t in
      let%lwt result = M.all_match_results t |> Lwt_seq.to_list in
      let actual = result |> List.map (fun (_, v) -> MR.matched v) in
      Alcotest.(check @@ list @@ list @@ pair int int)
        "queries"
        [ [ (1, 1) ]; [ (1, 2) ]; [ (1, 3) ]; [ (1, 4) ] ]
        actual;
      Lwt.return_unit )

let test2 =
  ( "apply previous query on add_candidate",
    `Quick,
    fun _ () ->
      let t = M.make () in
      M.apply_filter ~filter:(module Filter) ~query:"text" t;%lwt
      V.to_seq @@ candidates ()
      |> Lwt_seq.of_seq
      |> Lwt_seq.iter_s (fun v -> M.add_candidate ~candidate:v ~filter:(module Filter) t);%lwt
      let%lwt result = M.all_match_results t |> Lwt_seq.to_list in
      let actual = result |> List.map (fun (_, v) -> MR.matched v) in
      Alcotest.(check @@ list @@ list @@ pair int int)
        "queries"
        [ [ (1, 1) ]; [ (1, 2) ]; [ (1, 3) ]; [ (1, 4) ] ]
        actual;
      Lwt.return_unit )

let test3 =
  ( "all matched results are empty if no query applied previous",
    `Quick,
    fun _ () ->
      let t = M.make () in
      V.to_seq @@ candidates ()
      |> Lwt_seq.of_seq
      |> Lwt_seq.iter_s (fun v -> M.add_candidate ~candidate:v ~filter:(module Filter) t);%lwt
      let%lwt result = M.all_match_results t |> Lwt_seq.to_list in
      let actual = result |> List.map (fun (_, v) -> MR.matched v) in
      Alcotest.(check @@ list @@ list @@ pair int int) "queries" [ []; []; []; [] ] actual;
      Lwt.return_unit )

let test4 =
  ( "should return only matched indices",
    `Quick,
    fun _ () ->
      let t = M.make () in
      M.apply_filter ~filter:(module Filter) ~query:"text" t;%lwt
      V.to_seq @@ candidates ()
      |> Lwt_seq.of_seq
      |> Lwt_seq.iter_s (fun v -> M.add_candidate ~candidate:v ~filter:(module Even_match_filter) t);%lwt
      M.add_candidate ~candidate:(C.make ~id:5 ~text:"append") ~filter:(module Even_match_filter) t;%lwt
      let%lwt result =
        M.matched_results t |> Lwt_seq.map_s (fun (c, _) -> Lwt.return c.Candidate.id) |> Lwt_seq.to_list
      in
      Alcotest.(check @@ list @@ int) "queries" [ 2; 4 ] result;
      Lwt.return_unit )

let tests = [ test1; test2; test3; test4 ] |> List.map (fun (name, typ, case) -> Alcotest_lwt.test_case name typ case)
