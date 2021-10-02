type t
(** type of timestamp *)

val compare : t -> t -> int

val to_int64 : t -> int64
(** convert [t] to int64 *)

val of_int64 : int64 -> t
(** make [t] from int64 *)

val difference : before:t -> after:t -> t
(** [difference ~before ~after] get difference of timestamp between [before] and [after]. Result of this function offers
    minus timestamp if after is before from [before]. *)
