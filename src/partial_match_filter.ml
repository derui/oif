open Oif_lib
open Std
include Filter_intf

include struct
  let unique_name = "Partial match"

  let filter ~candidate ~query =
    let queries = Filter.split_query query |> List.filter ~f:(fun v -> String.length v > 0) in
    match queries with
    | [] -> Match_result.no_query ()
    | _ ->
        let match_result =
          List.fold_left
            ~f:(fun accum query ->
              let index = Ustring.index ~src:candidate.Candidate.text ~part:query in
              if index <> -1 then
                let end' = String.length query + index in
                (index, end') :: accum
              else accum)
            ~init:[] queries
          |> List.sort (fun (s1, _) (s2, _) -> Stdlib.compare s1 s2)
        in
        if List.length match_result <> List.length queries then Match_result.empty
        else Match_result.make ~matched:match_result
end
