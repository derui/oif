open CamomileLibrary
open Core.Std

module C = Types.Candidate
module ReIntf = CamomileLibraryDefault.Camomile.UReStr
module Re = ReIntf.Make (UTF8)

let matched_style = {LTerm_style.none with
  LTerm_style.bold = Some true;
  reverse = Some true;
}

let decorate_to_match text candidate =
  let regexp = Printf.sprintf "^.*\\(%s\\)" (C.text text) |> ReIntf.regexp in
  let regexp = Re.compile regexp in
  match Re.regexp_match ~sem:`First regexp (C.text candidate) 0 with
  | None -> candidate
  | Some (texts) -> begin match texts with
    | [|Some first;_|] ->
       let range = C.index_range_of_text candidate in
       let first = Re.SubText.first first |> Re.SubText.ur_index_of first in
       let f memo index = (index + first, matched_style) :: memo in
       let styles = List.fold ~init:[] ~f range in
       C.text candidate |> C.make ~styles
    | _ -> candidate
  end
