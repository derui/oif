module V = Oif_lib.Vector
module C = Oif.Candidate_selector

let tests =
  [
    ( "create initial instance",
      `Quick,
      fun () ->
        let t = C.make () in
        Alcotest.(check int) "initial" 0 @@ C.current_selected_index t );
    ( "can not move next if indices is empty",
      `Quick,
      fun () ->
        let t = C.make () in
        let indices = V.empty () in
        let t = C.select_next ~indices t in
        Alcotest.(check int) "initial" 0 @@ C.current_selected_index t );
    ( "can move next if indices is not empty and position is not reached at last point",
      `Quick,
      fun () ->
        let t = C.make () in
        let indices = V.make 5 0 in
        let t = C.select_next ~indices t in
        Alcotest.(check int) "initial" 1 @@ C.current_selected_index t );
    ( "can not move next if indices is not empty and position is reached at last point",
      `Quick,
      fun () ->
        let t = C.make () in
        let indices = V.make 2 0 in
        let t = C.select_next ~indices t in
        let t = C.select_next ~indices t in
        Alcotest.(check int) "initial" 1 @@ C.current_selected_index t );
    ( "can move previous if current point is not zero",
      `Quick,
      fun () ->
        let t = C.make () in
        let indices = V.make 3 0 in
        let t = C.select_next ~indices t in
        let t = C.select_next ~indices t in
        let t = C.select_previous t in
        Alcotest.(check int) "initial" 1 @@ C.current_selected_index t );
    ( "can not move previous if current point is zero",
      `Quick,
      fun () ->
        let t = C.make () in
        let t = C.select_previous t in
        Alcotest.(check int) "initial" 0 @@ C.current_selected_index t );
  ]
  |> List.map (fun (name, level, f) -> Alcotest_lwt.test_case_sync name level f)
