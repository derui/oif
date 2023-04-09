module W = Oif_lib.Virtual_window

let tests =
  [
    ( "calculated window return all 0 if any update",
      `Quick,
      fun () ->
        let w = W.create () |> W.calculate_window in
        Alcotest.(check int) "empty size" 0 w.size;
        Alcotest.(check int) "empty start index" 0 w.start_index;
        Alcotest.(check int) "empty end index" 0 w.end_index;
        Alcotest.(check int) "empty focused index" 0 w.focused_index );
    ( "update total rows when it bigger than before",
      `Quick,
      fun () ->
        let w = W.create () |> W.update_total_rows 100 |> W.update_view_port_size 10 |> W.calculate_window in
        Alcotest.(check int) "size" 10 w.size;
        Alcotest.(check int) "start index" 0 w.start_index;
        Alcotest.(check int) "end index" 9 w.end_index;
        Alcotest.(check int) "focused index" 0 w.focused_index );
    ( "update total rows when it smaller than before",
      `Quick,
      fun () ->
        let w =
          W.create () |> W.update_total_rows 100 |> W.update_view_port_size 10 |> W.update_total_rows 9
          |> W.calculate_window
        in
        Alcotest.(check int) "size" 9 w.size;
        Alcotest.(check int) "start index" 0 w.start_index;
        Alcotest.(check int) "end index" 8 w.end_index;
        Alcotest.(check int) "focused index" 0 w.focused_index );
    ( "move focusing row to over current focusing row",
      `Quick,
      fun () ->
        let w =
          W.create () |> W.update_total_rows 100 |> W.update_view_port_size 10 |> W.update_focused_row 5
          |> W.calculate_window
        in
        Alcotest.(check int) "size" 10 w.size;
        Alcotest.(check int) "start index" 0 w.start_index;
        Alcotest.(check int) "end index" 9 w.end_index;
        Alcotest.(check int) "focused index" 5 w.focused_index );
    ( "move focusing row to over current view port",
      `Quick,
      fun () ->
        let w =
          W.create () |> W.update_total_rows 100 |> W.update_view_port_size 10 |> W.update_focused_row 10
          |> W.calculate_window
        in
        Alcotest.(check int) "size" 10 w.size;
        Alcotest.(check int) "start index" 1 w.start_index;
        Alcotest.(check int) "end index" 10 w.end_index;
        Alcotest.(check int) "focused index" 10 w.focused_index );
    ( "move focusing row before current view port",
      `Quick,
      fun () ->
        let w =
          W.create () |> W.update_total_rows 100 |> W.update_view_port_size 10 |> W.update_focused_row 15
          |> W.update_focused_row 2 |> W.calculate_window
        in
        Alcotest.(check int) "size" 10 w.size;
        Alcotest.(check int) "start index" 2 w.start_index;
        Alcotest.(check int) "end index" 11 w.end_index;
        Alcotest.(check int) "focused index" 2 w.focused_index );
    ( "update view port size to bigger than before",
      `Quick,
      fun () ->
        let w =
          W.create () |> W.update_total_rows 10 |> W.update_view_port_size 5 |> W.update_focused_row 5
          |> W.update_view_port_size 8 |> W.calculate_window
        in
        Alcotest.(check int) "size" 8 w.size;
        Alcotest.(check int) "start index" 1 w.start_index;
        Alcotest.(check int) "end index" 8 w.end_index;
        Alcotest.(check int) "focused index" 5 w.focused_index );
    ( "update view port size if bigger than total rows",
      `Quick,
      fun () ->
        let w =
          W.create () |> W.update_total_rows 10 |> W.update_view_port_size 5 |> W.update_focused_row 5
          |> W.update_view_port_size 11 |> W.calculate_window
        in
        Alcotest.(check int) "size" 10 w.size;
        Alcotest.(check int) "start index" 0 w.start_index;
        Alcotest.(check int) "end index" 9 w.end_index;
        Alcotest.(check int) "focused index" 5 w.focused_index );
  ]
  |> List.map (fun (name, level, f) -> Alcotest_lwt.test_case_sync name level f)
