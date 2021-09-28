open Oif_lib
open Std
include Filter_intf

module Partial_match = struct
  let unique_name = "Partial match"

  let filter ~source ~text =
    let module I = Types.Info in
    let queries =
      Filter.split_query text
      |> List.filter ~f:(fun v -> String.length v > 0)
      |> List.map ~f:(fun v -> Printf.sprintf "^.*\\(%s\\)" v)
    in
    let candidates = source () |> Seq.filter_map (Matcher.query queries) in
    candidates
end

include Partial_match
