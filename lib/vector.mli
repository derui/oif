(** This module provides specialized array for candidate for efficient, and speed. *)

type 'a t
(** type of specialized array for candidate *)

val empty : unit -> 'a t
(** [empty ()] makes empty array *)

val make : int -> 'a -> 'a t
(** [make count value] makes new array with specified size *)

val init : int -> (int -> 'a) -> 'a t
(** [init count f] makes new array with specified size with function *)

val length : 'a t -> int
(** [length t] get length of [t] *)

val map : f:('a -> 'b) -> 'a t -> 'b t
(** [map ~f t] apply [f] to each candidate, and return new array. *)

val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
(** [mapi ~f t] apply [f] to each candidate with index, and return new array. *)

val unsafe_get : 'a t -> int -> 'a
(** [unsafe_get t index] get element at [index]. Raise [Invalid_argument] if give invalid index *)

val unsafe_set : 'a t -> int -> 'a -> unit
(** [unsafe_set t index value] set element at [index]. Raise [Invalid_argument] if give invalid index *)

val iter : f:('a -> unit) -> 'a t -> unit
(** [iter ~f t] apply [f] to each candidate, and do not return anything. *)

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
(** [iteri ~f t] apply [f] to each candidate with index, and do not return anything. *)

val of_seq : 'a Seq.t -> 'a t
(** [of_seq] make array from {!Seq} of candidate *)

val sub : 'a t -> int -> int -> 'a t
(** [sub t start len] get a slice of [t]. *)

val push : value:'a -> 'a t -> unit
(** [push ~value t] push a [value] to array. *)

val to_array : 'a t -> 'a array
(** [to_array t] get array of {!Candidate}. Notice: This function regenerate array in it, so performance is lesser than
    using [to_seq] *)

val to_seq : 'a t -> 'a Seq.t
(** [to_seq t] get {!Seq} of {!Candidate} contains array. *)
