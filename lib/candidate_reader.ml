type closer = unit -> unit

module type S = sig
  type t

  val make : unit -> t
  (** [make ()] make a new candidate reader *)

  val read_candidates_async : t -> fd:Unix.file_descr -> unit Lwt.t * closer
  (** [read_candidates_async t ~fd] make thread and closer to read candidates asynchronous *)

  val take_candidate : t -> string Lwt.t
  (** [take_candidate t] take a candidate from reader. This function waits to take least one candidate from reader. *)
end
