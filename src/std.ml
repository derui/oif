(* simple standard library *)

module List = struct
  include Stdlib.List

  let range : ?stop:[ `inclusive | `exclusive ] -> int -> int -> int list =
   fun ?(stop = `inclusive) from to_ ->
    if from < 0 || to_ < 0 then []
    else if from > to_ then []
    else
      let should_exit current to_ = match stop with `inclusive -> current > to_ | `exclusive -> current >= to_ in

      let rec loop current to_ accum =
        if should_exit current to_ then List.rev accum else loop (succ current) to_ (current :: accum)
      in
      loop from to_ []

  let map ~f lst = Stdlib.List.map f lst

  let fold_left ~f ~init lst = Stdlib.List.fold_left f init lst

  let find ~f lst = Stdlib.List.find_opt f lst

  let find_exn ~f lst = Stdlib.List.find f lst

  let iter ~f lst = Stdlib.List.iter f lst
end
