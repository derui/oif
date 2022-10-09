open Std
open CamomileLibraryDefault.Camomile
module DF = CamomileLibraryDefault
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)
include Filter_intf

let uchar_space = Uchar.of_char ' '

type accum = {
  characters : Buffer.t;
  queries : string list;
}

let split_query query =
  let query = String.trim query in
  let split query =
    Uutf.String.fold_utf_8
      (fun accum _ c ->
        let c = match c with `Malformed _ -> Uutf.u_rep | `Uchar c -> c in
        if c = uchar_space then
          if Buffer.length accum.characters > 0 then (
            let characters = Buffer.contents accum.characters in
            Buffer.clear accum.characters;
            { characters = accum.characters; queries = characters :: accum.queries })
          else accum
        else (
          Uutf.Buffer.add_utf_8 accum.characters c;
          accum))
      { characters = Buffer.create 16; queries = [] }
      query
  in

  let { queries; characters } = split query in
  let queries = if Buffer.length characters > 0 then Buffer.contents characters :: queries else queries in
  List.rev queries

let apply_matched queries candidate =
  match queries with
  | [] -> Match_result.no_query ()
  | _ ->
      let queries = List.map ~f:(fun v -> ReIntf.regexp v |> Re.compile) queries in
      let candidate = candidate.Candidate.text in
      let matched =
        List.fold_left
          ~f:(fun accum regexp ->
            let open Option.Let_syntax in
            let matches =
              let* texts = Re.search_forward ~sem:`Longest regexp candidate 0 in
              let* first = if Array.length texts < 2 then None else texts.(1) in
              let first' = Re.SubText.first first |> Re.SubText.ur_index_of first in
              let v = (first', first' + (String.length @@ Re.SubText.excerpt first)) in
              Some (v :: accum)
            in
            Option.value ~default:accum matches)
          ~init:[] queries
      in
      let matched = if List.length queries <> List.length matched then [] else matched in
      Match_result.make ~matched
