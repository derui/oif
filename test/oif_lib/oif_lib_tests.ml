let () =
  Alcotest.run "Oif"
    [
      ("Virtual window", Virtual_window_test.tests);
      ("Range", Range_test.tests);
      ("Line", Line_test.tests);
      ("Candidate", Candidate_test.tests);
      ("Std", Std_test.tests);
      ("Filter", Filter_test.tests);
      ("Timestamp", Timestamp_test.tests);
      ("Candidate array", Candidate_array_test.tests);
    ]
