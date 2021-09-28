module L = Oif_lib.Line
module F = Oif_lib.Filter

let tests =
  [
    ( "get empty list if query is empty",
      `Quick,
      fun () ->
        let ret = F.split_query "" in
        Alcotest.(check @@ list string) "empty" [] ret );
    ( "only passed query if query do not have space",
      `Quick,
      fun () ->
        let ret = F.split_query "query" in
        Alcotest.(check @@ list string) "query" [ "query" ] ret );
    ( "trimmed first and last spaces",
      `Quick,
      fun () ->
        let ret = F.split_query "  query  " in
        Alcotest.(check @@ list string) "query" [ "query" ] ret );
    ( "split query with space",
      `Quick,
      fun () ->
        let ret = F.split_query "query1 query2" in
        Alcotest.(check @@ list string) "queries" [ "query1"; "query2" ] ret );
    ( "split query with multiple spaces",
      `Quick,
      fun () ->
        let ret = F.split_query "query1    query2  query3" in
        Alcotest.(check @@ list string) "queries" [ "query1"; "query2"; "query3" ] ret );
  ]
