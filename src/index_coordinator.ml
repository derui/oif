open Oif_lib

type index = int

and size = int

and candidate_id = Candidate.id

and matcher_resolver = unit -> New_matcher.t

and matching = {
  candidate : Candidate.t;
  selected : bool;
  marked : bool;
  match_result : Match_result.t;
}

module Int_set = Set.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type t = {
  current_position : index;
  marked_indices : Int_set.t;
  matcher_resolver : matcher_resolver;
}

let make ~matcher = { current_position = 0; marked_indices = Int_set.empty; matcher_resolver = matcher }

let recalculate_index t =
  let matcher = t.matcher_resolver () in
  let matched_result_size = New_matcher.matched_count matcher in

  { t with current_position = max 0 @@ min t.current_position (matched_result_size - 1) }

let select_next t =
  let t = recalculate_index t in
  let size = t.matcher_resolver () |> New_matcher.matched_count in
  if size <= 0 then t
  else
    let next_selection = succ t.current_position in
    let allowed_index = pred size in
    { t with current_position = min allowed_index next_selection }

let select_previous t =
  let t = recalculate_index t in
  let next_position = pred t.current_position in
  { t with current_position = max 0 next_position }

let restrict_with_limit ~limit t = { t with current_position = max 0 @@ min t.current_position limit }

let toggle_mark_at_current_index t =
  let t = recalculate_index t in
  let marked_indices = t.marked_indices and idx = t.current_position in
  {
    t with
    marked_indices =
      (if Int_set.mem idx marked_indices then Int_set.remove idx marked_indices else Int_set.add idx marked_indices);
  }

let is_marked ~id { marked_indices; _ } = Int_set.mem id marked_indices

let iter_with_matching ~offset ~size ~f t =
  let matcher = t.matcher_resolver () in
  if New_matcher.matched_count matcher <= 0 then ()
  else
    New_matcher.matched_results ~offset ~size matcher
    |> Vector.iteri ~f:(fun index (candidate, match_result) ->
           let marked = is_marked ~id:index t in

           f index { candidate; marked; selected = t.current_position = index; match_result })

let selected_indices t =
  if Int_set.is_empty t.marked_indices then [ t.current_position ] else Int_set.to_seq t.marked_indices |> List.of_seq

let current_selected_index { current_position; _ } = current_position
