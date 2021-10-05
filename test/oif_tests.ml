let () =
  Alcotest.run "Oif"
    [
      ("Filter partial match", Partial_match_filter_test.tests);
      ("Events", Events_test.tests);
      ("Event hub", Event_hub_test.tests);
    ]
