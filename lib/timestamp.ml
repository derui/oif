type t = int64

let to_int64 t = t

let of_int64 t = t

let compare = Int64.compare

let difference ~before ~after = Int64.sub after before

module type Time = sig
  val now : unit -> int64
end

module type Instance = sig
  type timestamper

  val instance : timestamper

  val start : timestamper -> unit

  val timestamp : timestamper -> t
end

module type Timestamper = sig
  val make : unit -> (module Instance)
end

module Make (T : Time) : Timestamper = struct
  let make () =
    (module struct
      type timestamper = { mutable start : t }

      let instance = { start = of_int64 0L }

      let start instance =
        let start = T.now () in
        instance.start <- of_int64 start

      let timestamp { start } =
        let now = T.now () |> of_int64 in
        let diff = difference ~before:start ~after:now in
        diff
    end : Instance)
end
