open Oif_lib

type t

type matched_indices = int Vector.t

type index = int

type size = int

val make : unit -> t
(** [make ()] create new instance of [t]. *)

val select_next : indices:matched_indices -> t -> t
(** [select_next ~indices t] move selection into next position if allowed *)

val select_previous : t -> t
(** [select_previous t] move selection into previous position if allowed *)

val current_selected_index : t -> index
(** [current_selected_index t] get current position that is [t] selected *)

val restrict_with_limit : limit:size -> t -> t
(** [restrict_with_limit ~limit t] get new [t] with limit *)
