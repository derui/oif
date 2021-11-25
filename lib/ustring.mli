type t = string
(** type of ustring *)

val index : src:t -> part:t -> int
(** [index ~src ~part] find [part] from [src] as Unicode. Return [-1] if part is larger than [src] or not found [part]
    in [src] *)

val length : t -> int
(** [length t] get length of [t] as Unicode. *)
