type 'a t = {
  mutable array : 'a option array;
  mutable last_index : int;
}

let empty () = { array = Array.of_list []; last_index = -1 }

let make count value = { array = Array.make count (Some value); last_index = pred count }

let init count f = { array = Array.init count (fun i -> f i |> Option.some); last_index = pred count }

let expand t =
  let old = t.array in
  let old_length = Array.length old in
  t.array <- Array.init ((Array.length old * 2) + 1) (fun idx -> if old_length <= idx then None else old.(idx))

let length t = succ t.last_index

let unsafe_get t index =
  if t.last_index < index then raise (Invalid_argument "invalid index") else t.array.(index) |> Option.get

let unsafe_set t index value =
  if t.last_index < index then raise (Invalid_argument "invalid index") else t.array.(index) <- Some value

let map ~f t =
  let array = Array.map (fun v -> Option.map f v) t.array in
  let v' = empty () in
  v'.array <- array;
  v'.last_index <- t.last_index;
  v'

let mapi ~f t =
  let array = Array.mapi (fun i v -> Option.map (f i) v) t.array in
  let v' = empty () in
  v'.array <- array;
  v'.last_index <- t.last_index;
  v'

let of_seq seq =
  let array = seq |> Seq.map (fun v -> Some v) |> Array.of_seq in
  let last_index = Array.length array - 1 in
  { array; last_index }

let push ~value t =
  let index = succ t.last_index in
  let _ = if Array.length t.array <= index then expand t else () in
  t.array.(index) <- Some value;
  t.last_index <- index

let to_seq t =
  let iterator = ref 0 in
  Seq.unfold
    (fun t ->
      if !iterator > t.last_index then None
      else
        let value = t.array.(!iterator) in
        incr iterator;
        Some (Option.get value, t))
    t

let to_seq_with_index t =
  let iterator = ref 0 in
  Seq.unfold
    (fun t ->
      if !iterator > t.last_index then None
      else
        let value = t.array.(!iterator) in
        incr iterator;
        Some ((pred !iterator, Option.get value), t))
    t

let to_array t = to_seq t |> Array.of_seq

let iter ~f t = t |> to_seq |> Seq.iter f

let iteri ~f t = t |> to_seq_with_index |> Seq.iter (fun (index, value) -> f index value)

let sub t start len =
  let t = { array = Array.copy t.array; last_index = t.last_index } in
  { array = Array.sub t.array start len; last_index = pred len }
