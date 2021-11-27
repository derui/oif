type t = int * int

let merge ranges =
  let ranges' =
    List.sort
      (fun v1 v2 ->
        let compared = Stdlib.compare (fst v1) (fst v2) in
        if compared = 0 then Stdlib.compare (snd v1) (snd v2) else compared)
      ranges
  in
  let merged_ranges = Array.make (List.length ranges) None in
  let current_pos = ref 0 in
  List.iter
    (fun (s, e) ->
      let current = merged_ranges.(!current_pos) in
      match current with
      | None          -> merged_ranges.(!current_pos) <- Some (s, e)
      | Some (s', e') ->
          if s > e' then (
            incr current_pos;
            merged_ranges.(!current_pos) <- Some (s, e))
          else merged_ranges.(!current_pos) <- Some (s', max e e'))
    ranges';
  Array.to_seq merged_ranges |> Seq.filter_map (fun v -> v) |> List.of_seq
