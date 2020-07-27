type t = int * int

val merge : t list -> t list
(** [merge ranges] make merged range from [ranges]ranges. Merge algorithm is very simple that merge overlapped or
    adjacent range. *)
