open Oif_lib

module type S = sig
  val unique_name : string
  (** [unique_name] get the name of the Filter. This name must be unique in all filters. *)

  val filter : info:Types.Info.t -> text:string -> Candidate.t list
  (** [filter ~info ~text] return candidate list filtered by [text]user input. *)
end

(** Arguments for migemo filter. *)
module type Migemo_arg = sig
  val migemo : Migemocaml.Migemo.t
end
