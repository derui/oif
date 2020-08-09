open CamomileLibraryDefault.Camomile
module DF = CamomileLibraryDefault
module C = Types.Candidate
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)
module CaseMap = DF.Camomile.CaseMap.Make (UTF8)

let matched_style = { LTerm_style.none with LTerm_style.foreground = Some LTerm_style.cyan }

let decorate_to_match text candidate =
  let text = Option.value ~default:"" text in
  match UTF8.length text with
  | 0 -> Some (C.make candidate)
  | _ -> (
      let regexp = ReIntf.regexp text |> Re.compile in
      match Re.search_forward ~sem:`Longest regexp (CaseMap.lowercase candidate) 0 with
      | None       -> None
      | Some texts ->
          if Array.length texts > 1 then
            match texts.(1) with
            | None       -> None
            | Some first ->
                let first' = Re.SubText.first first |> Re.SubText.ur_index_of first in
                Some (C.make ~matched:[ (first', first' + (String.length @@ Re.SubText.excerpt first)) ] candidate)
          else None )
