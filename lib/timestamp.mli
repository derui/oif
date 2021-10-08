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

(** modules to get timestamp *)

module type Time = sig
  val now : unit -> int64
end

module type Timestamper = sig
  type timestamper

  val start : unit -> timestamper

  val timestamp : timestamper -> t
end

module Make (T : Time) : Timestamper
