open Oif_lib.Std

let begin_of_style style =
  let bold = style.LTerm_style.bold |> Option.fold ~none:[] ~some:(fun v -> [ LTerm_text.B_bold v ])
  and underline = style.LTerm_style.underline |> Option.fold ~none:[] ~some:(fun v -> [ LTerm_text.B_underline v ])
  and reverse = style.LTerm_style.reverse |> Option.fold ~none:[] ~some:(fun v -> [ LTerm_text.B_reverse v ])
  and foreground = style.LTerm_style.foreground |> Option.fold ~none:[] ~some:(fun v -> [ LTerm_text.B_fg v ])
  and background = style.LTerm_style.background |> Option.fold ~none:[] ~some:(fun v -> [ LTerm_text.B_bg v ]) in
  List.flatten [ bold; underline; reverse; foreground; background ]

let end_of_style style =
  let bold = style.LTerm_style.bold |> Option.fold ~none:[] ~some:(fun _ -> [ LTerm_text.E_bold ])
  and underline = style.LTerm_style.underline |> Option.fold ~none:[] ~some:(fun _ -> [ LTerm_text.E_underline ])
  and reverse = style.LTerm_style.reverse |> Option.fold ~none:[] ~some:(fun _ -> [ LTerm_text.E_reverse ])
  and foreground = style.LTerm_style.foreground |> Option.fold ~none:[] ~some:(fun _ -> [ LTerm_text.E_fg ])
  and background = style.LTerm_style.background |> Option.fold ~none:[] ~some:(fun _ -> [ LTerm_text.E_bg ]) in
  List.flatten [ background; foreground; reverse; underline; bold ]

let matched_style = { LTerm_style.none with LTerm_style.foreground = Some LTerm_style.cyan }

let selected_style = { LTerm_style.none with LTerm_style.bold = Some true }

let make_styled_candidate ~selected ~result candidate =
  let module O = Oif_lib in
  match O.Match_result.matched result with
  | []      -> LTerm_text.eval [ LTerm_text.S (O.Candidate.text candidate) ]
  | matched ->
      let merged_style_range = Oif_lib.Range.merge matched in
      let styled_texts =
        let text, rest, _ =
          List.fold_left
            ~f:(fun (acc, txt, prev_end) (s, e) ->
              let s, e = (s - prev_end, e - prev_end) in
              let before = String.sub txt 0 s
              and after = String.sub txt e (String.length txt - e)
              and substring = String.sub txt s (e - s) in
              let acc =
                List.flatten
                  [
                    [ LTerm_text.S before ];
                    begin_of_style matched_style;
                    [ LTerm_text.S substring ];
                    end_of_style matched_style;
                  ]
                :: acc
              in
              (acc, after, e))
            ~init:([], O.Candidate.text candidate, 0)
            merged_style_range
        in
        [ LTerm_text.S rest ] :: text |> List.rev |> List.flatten
      in
      let styled_text =
        if selected then List.concat [ begin_of_style selected_style; styled_texts; end_of_style selected_style ]
        else styled_texts
      in
      LTerm_text.eval styled_text
