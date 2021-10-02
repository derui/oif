type t

module type Time = sig
  val now : unit -> int64
end

val start : (module Time) -> t

val make_stamp : t -> Timestamp.t
