(** a small library for filter *)

val split_query : string -> string list

val apply_matched : string list -> Candidate.t -> Match_result.t

include module type of Filter_intf
