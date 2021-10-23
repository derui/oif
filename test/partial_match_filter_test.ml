module F = Oif.Partial_match_filter

let tests =
  let line text = Oif_lib.Line.make ~id:1 ~text in
  let to_check v =
    let text = Oif_lib.Candidate.text v in
    let matched = Oif_lib.Candidate.is_matched v in
    (text, matched)
  in
  [
    ( "match all lines with empty query",
      `Quick,
      fun () ->
        let ret = F.filter ~source:(fun () -> List.to_seq [ line "testing"; line "next" ]) ~text:"" in

        let ret = ret |> Seq.map Oif_lib.Candidate.text |> List.of_seq in
        Alcotest.(check @@ list string) "empty" [ "testing"; "next" ] ret );
    ( "return only matched candidates that is matched partially with single query",
      `Quick,
      fun () ->
        let ret = F.filter ~source:(fun () -> List.to_seq [ line "testing"; line "next" ]) ~text:"test" in

        let ret = ret |> Seq.map to_check |> List.of_seq in
        Alcotest.(check @@ list @@ pair string bool) "empty" [ ("testing", true); ("next", false) ] ret );
    ( "return only matched candidates that is matched with splitted queries",
      `Quick,
      fun () ->
        let ret = F.filter ~source:(fun () -> List.to_seq [ line "testing"; line "next" ]) ~text:"te ing" in

        let ret = ret |> Seq.map to_check |> List.of_seq in
        Alcotest.(check @@ list @@ pair string bool) "empty" [ ("testing", true); ("next", false) ] ret );
    ( "return empty result when query did not match all lines",
      `Quick,
      fun () ->
        let ret = F.filter ~source:(fun () -> List.to_seq [ line "testing"; line "next" ]) ~text:"foo" in

        let ret = ret |> Seq.map to_check |> List.of_seq in
        Alcotest.(check @@ list @@ pair string bool) "empty" [ ("testing", false); ("next", false) ] ret );
  ]
