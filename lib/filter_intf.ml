type source = unit -> Line.t Seq.t
(** signature to provide source *)

module type S = sig
  val unique_name : string
  (** [unique_name] get the name of the Filter. This name must be unique in all filters. *)

  val filter : source:source -> text:string -> Candidate.t Seq.t
  (** [filter ~source ~text] return candidate list filtered by [text]user input. *)
end
