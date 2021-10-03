(* simple standard library *)

let id v = v

module Seq = struct
  include Stdlib.Seq

  let filter ~f seq = Stdlib.Seq.filter f seq

  let range : ?stop:[ `inclusive | `exclusive ] -> int -> int -> int Seq.t =
   fun ?(stop = `inclusive) from to_ ->
    if from < 0 || to_ < 0 then Seq.empty
    else if from > to_ then Seq.empty
    else
      let rec loop current to_ accum =
        if current < to_ then accum else loop (pred current) to_ (Seq.cons current accum)
      in
      loop (if stop = `inclusive then to_ else pred to_) from Seq.empty
end

module List = struct
  include Stdlib.List

  let range ?(stop = `inclusive) from to_ = Seq.range ~stop from to_ |> List.of_seq

  let map ~f lst = Stdlib.List.map f lst

  let filter ~f lst = Stdlib.List.filter f lst

  let filter_map ~f lst = Stdlib.List.filter_map f lst

  let fold_left ~f ~init lst = Stdlib.List.fold_left f init lst

  let find ~f lst = Stdlib.List.find_opt f lst

  let find_exn ~f lst = Stdlib.List.find f lst

  let iter ~f lst = Stdlib.List.iter f lst
end

module Option = struct
  include Stdlib.Option

  let ( >>= ) v f = Option.bind v f

  module Let_syntax = struct
    let ( let* ) v f = Option.bind v f
  end
end

module Result = struct
  include Stdlib.Result

  let ( >>= ) v f = Result.bind v f

  module Let_syntax = struct
    let ( let* ) v f = Result.bind v f
  end
end
