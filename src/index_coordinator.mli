open Oif_lib

type t

type candidate_id = Candidate.id

type index = int

type size = int

type matcher_resolver = unit -> New_matcher.t

type matching = {
  candidate : Candidate.t;
  selected : bool;
  marked : bool;
  match_result : Match_result.t;
}

val make : matcher:matcher_resolver -> t
(** [make ~matcher] create new instance of [t]. *)

val select_next : t -> t Lwt.t
(** [select_next ~indices t] move selection into next position if allowed *)

val select_previous : t -> t Lwt.t
(** [select_previous t] move selection into previous position if allowed *)

val restrict_with_limit : limit:size -> t -> t
(** [restrict_with_limit ~limit t] get new [t] with limit *)

val iter_with_matching : offset:int -> size:size -> f:(matching -> unit) -> t -> unit Lwt.t
(** [iter_with_matching ~offset ~size ~f t] iterate matching result with [f] *)

val toggle_mark : id:candidate_id -> t -> t
(** [toggle_mark id t] toggle mark for [candidate] in [t] *)
