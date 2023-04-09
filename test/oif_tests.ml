let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "Oif"
       [
         ("Index coordinator", Index_coordinator_test.tests);
         ("Filter partial match", Partial_match_filter_test.tests);
         ("Events", Events_test.tests);
         ("Event hub", Event_hub_test.tests);
         ("Line separated reader", Line_separated_reader_test.tests);
       ]
