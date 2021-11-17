module M = Oif_lib.Match_result
module F = Oif.Partial_match_filter

let tests =
  let candidate text = Oif_lib.Candidate.make ~id:1 ~text in
  [
    ( "match all lines with empty query",
      `Quick,
      fun () ->
        let ret = F.filter ~candidate:(candidate "testing") ~query:"" in

        let ret = ret |> Oif_lib.Match_result.is_matched in
        Alcotest.(check @@ bool) "empty" true ret );
    ( "return only matched candidates that is matched partially with single query",
      `Quick,
      fun () ->
        let ret =
          [ candidate "testing"; candidate "next" ] |> List.map (fun v -> F.filter ~candidate:v ~query:"test")
        in

        let ret = ret |> List.map M.is_matched in
        Alcotest.(check @@ list bool) "empty" [ true; false ] ret );
    ( "return only matched candidates that is matched with splitted queries",
      `Quick,
      fun () ->
        let ret =
          [ candidate "testing"; candidate "next" ] |> List.map (fun v -> F.filter ~candidate:v ~query:"te ing")
        in

        let ret = ret |> List.map M.is_matched in
        Alcotest.(check @@ list bool) "empty" [ true; false ] ret );
    ( "return empty result when query did not match all lines",
      `Quick,
      fun () ->
        let ret = [ candidate "testing"; candidate "next" ] |> List.map (fun v -> F.filter ~candidate:v ~query:"foo") in

        let ret = ret |> List.map M.is_matched in
        Alcotest.(check @@ list bool) "empty" [ false; false ] ret );
  ]
