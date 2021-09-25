open Oif_lib

type t

val equal : t -> t -> bool

val empty : t

val is_empty : t -> bool

val toggle_mark : Candidate.t -> t -> t

val is_marked : Candidate.t -> t -> bool

val marked_lines : t -> int Seq.t
