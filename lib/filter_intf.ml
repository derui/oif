module type S = sig
  val unique_name : string
  (** [unique_name] get the name of the Filter. This name must be unique in all filters. *)

  val filter : candidate:Candidate.t -> query:string -> Match_result.t
  (** [filter ~source ~query] return candidate list filtered by [text]user input. *)
end
