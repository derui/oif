open CamomileLibraryDefault.Camomile
module DF = CamomileLibraryDefault
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)
include Filter_intf

let space_regexp = ReIntf.regexp "\\([^ ]+\\)" |> Re.compile

let split_query query =
  let query = String.trim query in
  let rec split query accum =
    match Re.search_forward space_regexp query 0 with
    | None   -> if String.length query > 0 then query :: accum else accum
    | Some g -> (
        match g.(1) with
        | None   -> accum
        | Some v ->
            let first = Re.SubText.first v |> Re.SubText.ur_index_of v
            and len = Re.SubText.excerpt v |> String.length in
            if len = 0 then accum
            else
              let query_len = String.length query in
              split (String.sub query (first + len) (query_len - (first + len))) (Re.SubText.excerpt v :: accum))
  in
  split query [] |> List.rev
