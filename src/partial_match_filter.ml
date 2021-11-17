open Oif_lib
open Std
include Filter_intf

module Partial_match = struct
  let unique_name = "Partial match"

  let filter ~candidate ~query =
    let queries =
      Filter.split_query query
      |> List.filter ~f:(fun v -> String.length v > 0)
      |> List.map ~f:(fun v -> Printf.sprintf "\\(%s\\)" v)
    in
    Filter.apply_matched queries candidate
end

include Partial_match
