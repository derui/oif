module type Time = sig
  val now : unit -> int64
end

type t = {
  start : Timestamp.t;
  time : (module Time);
}

let start (module T : Time) =
  let start = T.now () in
  { start = Timestamp.of_int64 start; time = (module T) }

let make_stamp { start; time = (module T) } =
  let now = T.now () |> Timestamp.of_int64 in
  let diff = Timestamp.difference ~before:start ~after:now in
  diff
