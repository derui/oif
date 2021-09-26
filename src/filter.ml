open Oif_lib
open Std
include Filter_intf
open CamomileLibraryDefault.Camomile
module DF = CamomileLibraryDefault
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)
module CaseMap = DF.Camomile.CaseMap.Make (UTF8)

let space_regexp = ReIntf.regexp "\\([^ ]+\\)" |> Re.compile

let split_query query =
  let rec split query accum =
    match Re.search_forward space_regexp query 0 with
    | None   -> query :: accum
    | Some g -> (
        match g.(1) with
        | None   -> accum
        | Some v ->
            let first = Re.SubText.first v |> Re.SubText.ur_index_of v
            and len = Re.SubText.excerpt v |> String.length in
            let query_len = String.length query in
            split (String.sub query (first + len) (query_len - (first + len))) (Re.SubText.excerpt v :: accum))
  in
  split query []

module Partial_match = struct
  let unique_name = "Partial match"

  let filter ~info ~text =
    let module I = Types.Info in
    let queries =
      split_query text
      |> List.filter ~f:(fun v -> String.length v > 0)
      |> List.map ~f:(fun v -> Printf.sprintf "^.*\\(%s\\)" v)
    in
    let candidates =
      List.map info.I.lines ~f:(Matcher.query queries) |> List.filter ~f:Option.is_some |> List.map ~f:Option.get
    in
    candidates
end

module Migemo (A : Migemo_arg) = struct
  let unique_name = "Migemo"

  let filter ~info ~text =
    let queries =
      split_query text
      |> List.filter ~f:(fun v -> String.length v > 0)
      |> List.map ~f:(fun query -> Migemocaml.Migemo.query ~query A.migemo)
    in

    let module I = Types.Info in
    let candidates =
      List.map info.I.lines ~f:(Matcher.query queries) |> List.filter ~f:Option.is_some |> List.map ~f:Option.get
    in
    candidates
end
