open Oif_lib

type matched_indices = int Vector.t

type index = int

type t = { current_position : index }

let make () = { current_position = 0 }

let select_next ~indices t =
  let size = Vector.length indices in
  if size <= 0 then t
  else
    let next_selection = succ t.current_position in
    let allowed_index = pred size in
    { current_position = min allowed_index next_selection }

let select_previous t =
  let next_position = pred t.current_position in
  { current_position = max 0 next_position }

let current_selected_index t = t.current_position
