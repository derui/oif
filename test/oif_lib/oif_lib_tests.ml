let () =
  Alcotest.run "Oif"
    [
      ("Virtual window", Virtual_window_test.tests);
      ("Range", Range_test.tests);
      ("Candidate", Candidate_test.tests);
      ("Std", Std_test.tests);
      ("Filter", Filter_test.tests);
      ("Match result", Match_result_test.tests);
      ("Timestamp", Timestamp_test.tests);
      ("Vector", Vector_test.tests);
      ("Matcher", Matcher_test.tests);
      ("Ustring", Ustring_test.tests);
    ]
