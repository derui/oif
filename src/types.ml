open CamomileLibraryDefault.Camomile
open Std

type matched = int * int

type selection = int * LTerm_style.t

type direction =
  | Next
  | Prev

module Line : sig
  type id = int

  type t = private {
    id : id;
    text : UTF8.t;
  }

  val make : id -> UTF8.t -> t
end = struct
  type id = int

  type t = {
    id : id;
    text : UTF8.t;
  }

  let make id text = { id; text }
end

module Candidate : sig
  type id = int

  type t = private {
    line : Line.t;
    matched : matched list;
  }

  val make : ?matched:matched list -> Line.t -> t

  val text : t -> UTF8.t

  val matched : t -> matched list

  val make_styled_text : bool -> t -> LTerm_text.t
end = struct
  type id = int

  type t = {
    line : Line.t;
    matched : matched list;
  }

  let make ?(matched = []) line = { line; matched }

  let text { line = { Line.text; _ }; _ } = text

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
    | [] -> LTerm_text.eval [ LTerm_text.S (text t) ]
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
              ~init:([], text t, 0)
              merged_style_range
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

module Item_marker : sig
  type t

  val equal : t -> t -> bool

  val empty : t

  val is_empty : t -> bool

  val toggle_mark : Candidate.t -> t -> t

  val is_marked : Candidate.t -> t -> bool

  val marked_lines : t -> int Seq.t
end = struct
  module Int_set = Set.Make (struct
    type t = int

    let compare = Stdlib.compare
  end)

  type t = { marked_lines : Int_set.t }

  let equal v1 v2 = Int_set.equal v1.marked_lines v2.marked_lines

  let empty = { marked_lines = Int_set.empty }

  let is_empty { marked_lines } = Int_set.is_empty marked_lines

  let toggle_mark candidate { marked_lines } =
    let line_id = candidate.Candidate.line.id in
    {
      marked_lines =
        ( if Int_set.mem line_id marked_lines then Int_set.remove line_id marked_lines
        else Int_set.add line_id marked_lines );
    }

  let marked_lines { marked_lines } = Int_set.to_seq marked_lines

  let is_marked candidate { marked_lines } = Int_set.mem candidate.Candidate.line.id marked_lines
end

(* Infomations to filtering candidates. *)
module Info = struct
  type t = {
    mutable current_candidate : string option;
    mutable lines : Line.t list;
  }

  let empty = { current_candidate = None; lines = [] }

  let to_candidates info = List.map info.lines ~f:(fun v -> Candidate.make v)

  let init lines = { empty with lines = List.mapi (fun i l -> Line.make i l) lines }
end
