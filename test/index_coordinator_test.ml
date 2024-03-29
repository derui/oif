module Candidate = Oif_lib.Candidate
module Matcher = Oif_lib.New_matcher
module Match_result = Oif_lib.Match_result
module V = Oif_lib.Vector
module C = Oif.Index_coordinator

let candidate_t = Alcotest.testable Candidate.pp ( = )

module F = struct
  let unique_name = "test"

  let filter ~candidate:_ ~query:_ = Match_result.no_query ()
end

let to_candidates list = List.map (fun (id, text) -> Candidate.make ~id ~text) list

let test1 =
  ( "create initial instance",
    `Quick,
    fun _ () ->
      let t = C.make ~matcher:(fun () -> Matcher.make ()) in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun _ v -> ret := v.candidate :: !ret) ~offset:0 ~size:0 t;
      Alcotest.(check @@ list candidate_t) "initial" [] !ret;
      Lwt.return_unit )

let test2 =
  ( "can not move next if candidates is least two",
    `Quick,
    fun _ () ->
      let matcher = Matcher.make () in
      let t = C.make ~matcher:(fun () -> matcher) in
      let candidate = Candidate.make ~id:1 ~text:"foo" in
      Matcher.add_candidate ~candidate ~filter:(module F) matcher;%lwt
      let t = C.select_next t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun _ v -> ret := v.selected :: !ret) ~offset:0 ~size:1 t;
      Alcotest.(check @@ list bool) "initial" [ true ] !ret;
      Lwt.return_unit )

let test3 =
  ( "can move next if indices is not empty and position is not reached at last point",
    `Quick,
    fun _ () ->
      let matcher = Matcher.make () in
      let t = C.make ~matcher:(fun () -> matcher) in
      let candidate = Candidate.make ~id:1 ~text:"foo" in
      Matcher.add_candidate ~candidate ~filter:(module F) matcher;%lwt
      let t = C.select_next t in
      let t = C.select_next t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun _ v -> ret := v.selected :: !ret) ~offset:0 ~size:1 t;
      Alcotest.(check @@ list bool) "initial" [ true ] !ret;
      Lwt.return_unit )

let test4 =
  ( "can not move next if indices is not empty and position is reached at last point",
    `Quick,
    fun _ () ->
      let matcher = Matcher.make () in
      let t = C.make ~matcher:(fun () -> matcher) in
      let candidates = to_candidates [ (1, "text"); (2, "foo") ] in
      candidates |> Lwt_list.iter_s (fun value -> Matcher.add_candidate ~candidate:value ~filter:(module F) matcher);%lwt
      let t = C.select_next t in
      let t = C.select_next t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun _ v -> ret := v.selected :: !ret) ~offset:0 ~size:2 t;
      Alcotest.(check @@ list bool) "initial" [ true; false ] !ret;
      Lwt.return_unit )

let test5 =
  ( "can move previous if current point is not zero",
    `Quick,
    fun _ () ->
      let matcher = Matcher.make () in
      let t = C.make ~matcher:(fun () -> matcher) in
      let candidates = to_candidates [ (1, "text"); (2, "foo") ] in
      candidates |> Lwt_list.iter_s (fun value -> Matcher.add_candidate ~candidate:value ~filter:(module F) matcher);%lwt

      let t = C.select_next t in
      let t = C.select_next t in
      let t = C.select_previous t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun _ v -> ret := v.selected :: !ret) ~offset:0 ~size:2 t;
      Alcotest.(check @@ list bool) "initial" [ true; false ] (!ret |> List.rev);
      Lwt.return_unit )

let test6 =
  ( "can not move previous if current point is zero",
    `Quick,
    fun _ () ->
      let matcher = Matcher.make () in
      let t = C.make ~matcher:(fun () -> matcher) in
      let t = C.select_previous t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun _ v -> ret := v.selected :: !ret) ~offset:0 ~size:0 t;
      Alcotest.(check @@ list bool) "initial" [] !ret;
      Lwt.return_unit )

let test7 =
  ( "do not throw error if size and offset are greater than array size",
    `Quick,
    fun _ () ->
      let matcher = Matcher.make () in
      let t = C.make ~matcher:(fun () -> matcher) in
      let t = C.select_previous t in
      let ret = ref [] in
      C.iter_with_matching ~f:(fun _ v -> ret := v.selected :: !ret) ~offset:0 ~size:1 t;
      Alcotest.(check @@ list bool) "initial" [] !ret;
      Lwt.return_unit )

let test8 =
  ( "recalculate current position",
    `Quick,
    fun _ () ->
      let matcher = ref @@ Matcher.make () in
      let t = C.make ~matcher:(fun () -> !matcher) in
      let candidates = to_candidates [ (1, "text"); (2, "foo") ] in
      candidates |> Lwt_list.iter_s (fun value -> Matcher.add_candidate ~candidate:value ~filter:(module F) !matcher);%lwt
      let t = C.select_next t in
      matcher := Matcher.make ();
      let t = C.select_next t in
      Alcotest.(check @@ int) "index" 0 (C.current_selected_index t);
      Lwt.return_unit )

let test9 =
  ( "recalculate current position with some matching",
    `Quick,
    fun _ () ->
      let matcher = ref @@ Matcher.make () in
      let t = C.make ~matcher:(fun () -> !matcher) in
      let candidates = to_candidates [ (1, "text"); (2, "foo"); (3, "bar") ] in
      candidates |> Lwt_list.iter_s (fun value -> Matcher.add_candidate ~candidate:value ~filter:(module F) !matcher);%lwt
      let t = C.select_next t |> C.select_next in
      matcher := Matcher.make ();
      let candidates = to_candidates [ (1, "text"); (2, "foo") ] in
      candidates |> Lwt_list.iter_s (fun value -> Matcher.add_candidate ~candidate:value ~filter:(module F) !matcher);%lwt
      let t = C.select_next t in
      Alcotest.(check @@ int) "index" 1 (C.current_selected_index t);
      Lwt.return_unit )

let test10 =
  ( "marked indices are sorted by natural",
    `Quick,
    fun _ () ->
      let matcher = ref @@ Matcher.make () in
      let t = C.make ~matcher:(fun () -> !matcher) in
      let candidates = to_candidates [ (1, "text"); (2, "foo"); (3, "bar") ] in
      candidates |> Lwt_list.iter_s (fun value -> Matcher.add_candidate ~candidate:value ~filter:(module F) !matcher);%lwt
      let t =
        t |> C.select_next |> C.select_next |> C.toggle_mark_at_current_index |> C.select_previous
        |> C.toggle_mark_at_current_index
      in
      Alcotest.(check @@ list @@ int) "index" [ 1; 2 ] (C.selected_indices t);
      Lwt.return_unit )

let tests =
  [ test1; test2; test3; test4; test5; test6; test7; test8; test9; test10 ]
  |> List.map (fun (name, level, f) -> Alcotest_lwt.test_case name level f)
