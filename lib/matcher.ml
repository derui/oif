open Std
open CamomileLibraryDefault.Camomile
module DF = CamomileLibraryDefault
module C = Candidate
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)
module CaseMap = DF.Camomile.CaseMap.Make (UTF8)

type query = string

let apply_matched queries line =
  match queries with
  | [] -> C.make line
  | _  ->
      let queries = List.map ~f:(fun v -> ReIntf.regexp v |> Re.compile) queries in
      let candidate = line.Line.text in
      let matched =
        List.fold_left
          ~f:(fun accum regexp ->
            let open Option.Let_syntax in
            let matches =
              let* texts = Re.search_forward ~sem:`Longest regexp (CaseMap.lowercase candidate) 0 in
              let* first = if Array.length texts < 2 then None else texts.(1) in
              let first' = Re.SubText.first first |> Re.SubText.ur_index_of first in
              let v = (first', first' + (String.length @@ Re.SubText.excerpt first)) in
              Some (v :: accum)
            in
            Option.value ~default:accum matches)
          ~init:[] queries
      in
      let matched = if List.length queries <> List.length matched then [] else matched in
      C.make ~matched ~filtered:true line
