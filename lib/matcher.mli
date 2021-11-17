type t

val make : candidates:Candidate.t Vector.t ref -> t
(** [make ~candidates] makes new matcher [t] with candidate vector *)

val apply_filter : filter:(module Filter_intf.S) -> query:string -> t -> unit
(** [apply_filter ~filter ~query t] applies filter with [query]. This function has side effect in [t]. *)

val match_results : t -> Match_result.t Vector.t
(** [match_results t] returns match result on previous [apply_filter] *)

val matched_indices : t -> int Vector.t
(** [matched_indices t] returns only indices on previous apply_filter *)
