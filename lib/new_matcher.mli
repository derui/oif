type t

val make : unit -> t
(** [make ()] makes new matcher [t]. *)

val add_candidate : candidate:Candidate.t -> filter:(module Filter_intf.S) -> t -> unit Lwt.t
(** [add_candidate ~candidate ~filter t] append [candidate] and update matching result for [candidate] by [filter]. *)

val apply_filter : filter:(module Filter_intf.S) -> query:string -> t -> unit Lwt.t
(** [apply_filter ~filter ~query t] applies filter with [query]. This function has side effect in [t]. *)

val all_match_results : t -> (Candidate.t * Match_result.t) Vector.t
(** [all_match_results t] returns match result with candidate *)

val matched_results : offset:int -> size:int -> t -> (Candidate.t * Match_result.t) Vector.t
(** [matched_results ~offset ~size t] returns only matched result on previous apply_filter or add_candidate *)

val matched_count : t -> int
(** [matched_count t] get current count of matched candidate. This function works in constant time *)

val total_candidate_count : t -> int
(** [total_candidate_count t] returns count of candidate. *)
