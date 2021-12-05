open Oif_lib

type t
(** the type of item marker *)

type candidate_id = Candidate.id

val equal : t -> t -> bool
(** [equal v1 v2] is equal [v1] and [v2] *)

val empty : t
(** [empty] get instance of [t] *)

val is_empty : t -> bool
(** [is_empty t] get empty or not [t] *)

val toggle_mark : index:candidate_id -> t -> t
(** [toggle_mark candidate t] toggle mark for [candidate] in [t] *)

val is_marked : index:candidate_id -> t -> bool
(** [is_marked candidate t] get current status of [candidate] in [t] *)

val marked_indices : t -> candidate_id Seq.t
(** [marked_indices t] get id of lines marked in [t] *)
