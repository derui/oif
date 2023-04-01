module Candidate = Oif_lib.Candidate
module Matcher = Oif_lib.Matcher
module Match_result = Oif_lib.Match_result
module V = Oif_lib.Vector
module C = Oif.Index_coordinator

let candidate_t = Alcotest.testable Candidate.pp ( = )

module F = struct
  let unique_name = "test"

  let filter ~candidate:_ ~query:_ = Match_result.empty
end

let to_candidates list = List.map (fun (id, text) -> Candidate.make ~id ~text) list

let test1 =
  ( "create initial instance",
    `Quick,
    fun () ->
      let candidates = ref @@ V.empty () in
      let t = C.make ~matcher:(fun () -> Matcher.make ~candidates) ~candidates:(fun () -> !candidates) in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun v -> ret := v.candidate :: !ret) ~offset:0 ~size:0 t;
      Alcotest.(check @@ list candidate_t) "initial" [] !ret )

let test2 =
  ( "can not move next if candidates is least two",
    `Quick,
    fun () ->
      let candidates = ref @@ V.empty () in
      let matcher = Matcher.make ~candidates in
      let t = C.make ~matcher:(fun () -> matcher) ~candidates:(fun () -> !candidates) in
      let candidate = Candidate.make ~id:1 ~text:"foo" in
      V.push ~value:candidate !candidates;
      let t = C.select_next t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun v -> ret := v.selected :: !ret) ~offset:0 ~size:1 t;
      Alcotest.(check @@ list bool) "initial" [ true ] !ret )

let test3 =
  ( "can move next if indices is not empty and position is not reached at last point",
    `Quick,
    fun () ->
      let candidates = ref @@ V.empty () in
      let matcher = Matcher.make ~candidates in
      let t = C.make ~matcher:(fun () -> matcher) ~candidates:(fun () -> !candidates) in
      let candidate = Candidate.make ~id:1 ~text:"foo" in
      V.push ~value:candidate !candidates;
      let t = C.select_next t in
      let t = C.select_next t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun v -> ret := v.selected :: !ret) ~offset:0 ~size:1 t;
      Alcotest.(check @@ list bool) "initial" [ true ] !ret )

let test4 =
  ( "can not move next if indices is not empty and position is reached at last point",
    `Quick,
    fun () ->
      let candidate_vec = ref @@ V.empty () in
      let matcher = Matcher.make ~candidates:candidate_vec in
      let t = C.make ~matcher:(fun () -> matcher) ~candidates:(fun () -> !candidate_vec) in
      let candidates = to_candidates [ (1, "text"); (2, "foo") ] in
      List.iter (fun value -> V.push ~value !candidate_vec) candidates;
      let t = C.select_next t in
      let t = C.select_next t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun v -> ret := v.selected :: !ret) ~offset:0 ~size:2 t;
      Alcotest.(check @@ list bool) "initial" [ true; false ] !ret )

let test5 =
  ( "can move previous if current point is not zero",
    `Quick,
    fun () ->
      let candidate_vec = ref @@ V.empty () in
      let matcher = Matcher.make ~candidates:candidate_vec in
      let t = C.make ~matcher:(fun () -> matcher) ~candidates:(fun () -> !candidate_vec) in
      let candidates = to_candidates [ (1, "text"); (2, "foo") ] in
      List.iter (fun value -> V.push ~value !candidate_vec) candidates;
      let t = C.select_next t in
      let t = C.select_next t in
      let t = C.select_previous t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun v -> ret := v.selected :: !ret) ~offset:0 ~size:2 t;
      Alcotest.(check @@ list bool) "initial" [ true; false ] (!ret |> List.rev) )

let test6 =
  ( "can not move previous if current point is zero",
    `Quick,
    fun () ->
      let candidate_vec = ref @@ V.empty () in
      let matcher = Matcher.make ~candidates:candidate_vec in
      let t = C.make ~matcher:(fun () -> matcher) ~candidates:(fun () -> !candidate_vec) in
      let t = C.select_previous t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun v -> ret := v.selected :: !ret) ~offset:0 ~size:0 t;
      Alcotest.(check @@ list bool) "initial" [] !ret )

let tests =
  [ test1; test2; test3; test4; test5; test6 ]
  |> List.map (fun (name, level, f) -> Alcotest_lwt.test_case_sync name level f)
