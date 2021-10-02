type t = int64

let to_int64 t = t

let of_int64 t = t

let compare = Int64.compare

let difference ~before ~after = Int64.sub after before
