(** This module provides specialized array for candidate for efficient, and speed. *)

type t
(** type of specialized array for candidate *)

val empty : unit -> t
(** [empty ()] makes empty array *)

val length : t -> int
(** [length t] get length of [t] *)

val map : f:(Candidate.t -> Candidate.t) -> t -> t
(** [map ~f t] apply [f] to each candidate, and return new array. *)

val unsafe_get : t -> int -> Candidate.t
(** [unsafe_get t index] get element at [index]. Raise [Invalid_argument] if give invalid index *)

val iter : f:(Candidate.t -> unit) -> t -> unit
(** [iter ~f t] apply [f] to each candidate, and do not return anything. *)

val iteri : f:(int -> Candidate.t -> unit) -> t -> unit
(** [iteri ~f t] apply [f] to each candidate with index, and do not return anything. *)

val of_seq : Candidate.t Seq.t -> t
(** [of_seq] make array from {!Seq} of candidate *)

val sub : t -> int -> int -> t
(** [sub t start len] get a slice of [t]. *)

val push : value:Candidate.t -> t -> unit
(** [push ~value t] push a [value] to array. *)

val to_array : t -> Candidate.t array
(** [to_array t] get array of {!Candidate}. Notice: This function regenerate array in it, so performance is lesser than
    using [to_seq] *)

val to_seq : t -> Candidate.t Seq.t
(** [to_seq t] get {!Seq} of {!Candidate} contains array. *)
