open CamomileLibraryDefault.Camomile
module DF = CamomileLibraryDefault
module C = Types.Candidate
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)
module CaseMap = DF.Camomile.CaseMap.Make (UTF8)

let matched_style = { LTerm_style.none with LTerm_style.foreground = Some LTerm_style.cyan }

let decorate_to_match queries line =
  match queries with
  | [] -> Some (C.make line)
  | _  ->
      let queries = List.map (fun v -> ReIntf.regexp v |> Re.compile) queries in
      let candidate = line.Types.Line.text in
      let matched =
        List.fold_left
          (fun accum regexp ->
            match Re.search_forward ~sem:`Longest regexp (CaseMap.lowercase candidate) 0 with
            | None       -> accum
            | Some texts -> (
                if Array.length texts < 2 then accum
                else
                  match texts.(1) with
                  | None       -> accum
                  | Some first ->
                      let first' = Re.SubText.first first |> Re.SubText.ur_index_of first in
                      (first', first' + (String.length @@ Re.SubText.excerpt first)) :: accum ))
          [] queries
      in
      if List.length queries <> List.length matched then None else Some (C.make ~matched line)
