open Oif_lib

type index = int

type size = int

type candidate_id = Candidate.id

type matcher_resolver = unit -> Matcher.t

type candidates_resolver = unit -> Candidate.t Vector.t

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
  candidates_resolver : candidates_resolver;
}

let make ~matcher ~candidates =
  { current_position = 0; marked_indices = Int_set.empty; matcher_resolver = matcher; candidates_resolver = candidates }

let select_next t =
  let indices = t.matcher_resolver () |> Matcher.matched_indices in
  let size = Vector.length indices in
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
  let matcher = t.matcher_resolver () and candidates = t.candidates_resolver () in
  let matched_indices = Matcher.matched_indices matcher in
  let match_results = Matcher.match_results matcher in
  if Vector.length matched_indices <= 0 then ()
  else
    Vector.sub matched_indices offset size
    |> Vector.iteri ~f:(fun index matched_index ->
           let candidate = Vector.unsafe_get candidates matched_index
           and match_result =
             if Vector.length match_results < Vector.length matched_indices then Match_result.empty
             else Vector.unsafe_get match_results matched_index
           in
           let marked = is_marked ~id:matched_index t in

           f { candidate; marked; selected = t.current_position = index; match_result })
