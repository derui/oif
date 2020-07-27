open CamomileLibraryDefault.Camomile
open Std

type matched = int * int

type selection = int * LTerm_style.t

type direction =
  | Next
  | Prev

module Candidate : sig
  type t = private {
    text : UTF8.t;
    matched : matched list;
  }

  val make : ?matched:matched list -> UTF8.t -> t

  val text : t -> UTF8.t

  val matched : t -> matched list

  val make_styled_text : bool -> t -> LTerm_text.t
end = struct
  type t = {
    text : UTF8.t;
    matched : matched list;
  }

  let make ?(matched = []) text = { text; matched }

  let text { text; _ } = text

  let matched { matched; _ } = matched

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

  let make_styled_text selected t =
    match t.matched with
    | [] -> LTerm_text.eval [ LTerm_text.S t.text ]
    | _  ->
        let merged_style_range = Oif_lib.Range.merge t.matched in
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
              ~init:([], t.text, 0) merged_style_range
          in
          [ LTerm_text.S rest ] :: text |> List.rev |> List.flatten
        in
        let styled_text =
          if selected then List.concat [ begin_of_style selected_style; styled_texts; end_of_style selected_style ]
          else styled_texts
        in
        LTerm_text.eval styled_text
end

type candidates = Candidate.t list

(* The candidates for filtering *)

(* Infomations to filtering candidates. *)
module Info = struct
  type t = {
    mutable current_candidate : string option;
    mutable lines : UTF8.t list;
  }

  let empty = { current_candidate = None; lines = [] }

  let to_candidates info = List.map info.lines ~f:Candidate.make

  let init lines = { empty with lines }
end
