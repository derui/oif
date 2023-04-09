open Oif_lib

type t

and candidate_id = Candidate.id

and index = int

and size = int

and matcher_resolver = unit -> New_matcher.t

and matching = {
  candidate : Candidate.t;
  selected : bool;
  marked : bool;
  match_result : Match_result.t;
}

val make : matcher:matcher_resolver -> t
(** [make ~matcher] create new instance of [t]. *)

val select_next : t -> t
(** [select_next ~indices t] move selection into next position if allowed *)

val select_previous : t -> t
(** [select_previous t] move selection into previous position if allowed *)

val recalculate_index : t -> t
(** [recalculate_index t] get new [t] that is recalculated index by matcher *)

val restrict_with_limit : limit:size -> t -> t
(** [restrict_with_limit ~limit t] get new [t] with limit *)

val iter_with_matching : offset:int -> size:size -> f:(index -> matching -> unit) -> t -> unit
(** [iter_with_matching ~offset ~size ~f t] iterate matching result with [f] *)

val toggle_mark_at_current_index : t -> t
(** [toggle_mark_at_current_index t] toggle mark for [candidate] in [t] *)

val selected_indices : t -> index list
(** [selected_indices t] get indices current marked or selected. *)

val current_selected_index : t -> index
(** [current_selected_index t] returns an index that user selected *)
