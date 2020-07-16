module W = Oif_lib.Virtual_window

let tests =
  [
    ( "calculated window return all 0 if any update",
      `Quick,
      fun () ->
        let w = W.create () |> W.calculate_window in
        Alcotest.(check int) "empty size" w.size 0;
        Alcotest.(check int) "empty start index" w.start_index 0;
        Alcotest.(check int) "empty end index" w.end_index 0;
        Alcotest.(check int) "empty focused index" w.focused_index 0 );
  ]
