open CamomileLibrary
open Std
module DF = CamomileLibraryDefault
module C = Types.Candidate
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)
module CaseMap = DF.Camomile.CaseMap.Make (UTF8)

let matched_style = { LTerm_style.none with LTerm_style.reverse = Some true }

let decorate_to_match text candidate =
  match UTF8.length text with
  | 0 -> Some (C.make candidate)
  | _ -> (
      let regexp = Printf.sprintf "^.*\\(%s\\)" (CaseMap.lowercase text) |> ReIntf.regexp in
      let regexp = Re.compile regexp in
      match Re.regexp_match ~sem:`First regexp (CaseMap.lowercase candidate) 0 with
      | None       -> None
      | Some texts -> (
          match texts with
          | [| Some _; Some first |] ->
              let range = Re.SubText.length first |> List.range ~stop:`exclusive 0 in
              let first = Re.SubText.first first |> Re.SubText.ur_index_of first in
              let f memo index = (index + first, matched_style) :: memo in
              let styles = List.fold_left ~f ~init:[] range in
              Some (C.make ~styles candidate)
          | _                        -> None ) )
