type t = int64

let to_int64 t = t

let of_int64 t = t

let compare = Int64.compare

let difference ~before ~after = Int64.sub after before

module type Time = sig
  val now : unit -> int64
end

module type Timestamper = sig
  type timestamper

  val start : unit -> timestamper

  val timestamp : timestamper -> t
end

module Make (T : Time) : Timestamper = struct
  type timestamper = {
    start : t;
    time : (module Time);
  }

  let start () =
    let start = T.now () in
    { start = of_int64 start; time = (module T) }

  let timestamp { start; time = (module T) } =
    let now = T.now () |> of_int64 in
    let diff = difference ~before:start ~after:now in
    diff
end
