open Oif_lib

type index = int

type size = int

type candidate_id = Candidate.id

type matcher_resolver = unit -> New_matcher.t

type matching = {
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

let current_selected_index t = t.current_position

let select_next t =
  let indices = t.matcher_resolver () |> New_matcher.matched_results |> List.of_seq in
  let size = List.length indices in
  if size <= 0 then t
  else
    let next_selection = succ t.current_position in
    let allowed_index = pred size in
    { t with current_position = min allowed_index next_selection }

let select_previous t =
  let next_position = pred t.current_position in
  { t with current_position = max 0 next_position }

let restrict_with_limit ~limit t = { t with current_position = max 0 @@ min t.current_position limit }

let toggle_mark ~id t =
  let marked_indices = t.marked_indices in
  {
    t with
    marked_indices =
      (if Int_set.mem id marked_indices then Int_set.remove id marked_indices else Int_set.add id marked_indices);
  }

let is_marked ~id { marked_indices; _ } = Int_set.mem id marked_indices

let iter_with_matching ~offset ~size ~f t =
  let matcher = t.matcher_resolver () in
  let matched_results = New_matcher.matched_results matcher |> List.of_seq |> Array.of_list in
  if Array.length matched_results <= 0 then ()
  else
    Array.sub matched_results offset (min (Array.length matched_results - offset) size)
    |> Array.iteri (fun index (candidate, match_result) ->
           let marked = is_marked ~id:index t in

           f index { candidate; marked; selected = t.current_position = index; match_result })

let selected_indices t =
  if Int_set.is_empty t.marked_indices then [ current_selected_index t ]
  else Int_set.to_seq t.marked_indices |> List.of_seq
