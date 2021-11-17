type matched = int * int [@@deriving show, eq]

type t [@@deriving show, eq]

val empty : t
(** [empty] get empty matched *)

val make : matched:matched list -> t
(** [make ~matched] make a new [t] *)

val no_query : unit -> t
(** [no_query ()] make a new [t] when no query *)

val matched : t -> matched list

val is_matched : t -> bool
(** [is_matched t] return [t] contains matched information or not *)
