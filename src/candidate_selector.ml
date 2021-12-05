open Oif_lib

type matched_indices = int Vector.t

type index = int

and size = int

and candidate_id = Candidate.id

module Int_set = Set.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type t = {
  current_position : index;
  marked_indices : Int_set.t;
}

let make () = { current_position = 0; marked_indices = Int_set.empty }

let select_next ~indices t =
  let size = Vector.length indices in
  if size <= 0 then t
  else
    let next_selection = succ t.current_position in
    let allowed_index = pred size in
    { t with current_position = min allowed_index next_selection }

let select_previous t =
  let next_position = pred t.current_position in
  { t with current_position = max 0 next_position }

let current_selected_index t = t.current_position

let restrict_with_limit ~limit t = { t with current_position = max 0 @@ min t.current_position limit }

let toggle_mark ~id t =
  let marked_indices = t.marked_indices in
  {
    t with
    marked_indices =
      (if Int_set.mem id marked_indices then Int_set.remove id marked_indices else Int_set.add id marked_indices);
  }

let marked_indices { marked_indices; current_position } =
  let marked = Int_set.to_seq marked_indices |> List.of_seq in
  match marked with [] -> [ current_position ] | _ -> marked

let is_marked ~id t = Int_set.mem id t.marked_indices
