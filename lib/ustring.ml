type t = string

let string_to_uchar_list s =
  Uutf.String.fold_utf_8 (fun accum _ c -> match c with `Malformed _ -> accum | `Uchar c -> c :: accum) [] s
  |> List.rev

let index ~src ~part =
  let src = string_to_uchar_list src and part = string_to_uchar_list part in

  let src_len = List.length src and part_len = List.length part in
  if src_len < part_len then -1
  else if part_len = 0 then -1
  else
    let rec find src part count index =
      if index <> -1 then index
      else
        match (src, part) with
        | _, []              -> index
        | [], _              -> -1
        | v1 :: r1, v2 :: r2 ->
            if Uchar.equal v1 v2 then find r1 r2 (succ count) (if index <> -1 then index else count)
            else find r1 part (succ count) index
    in
    find src part 0 (-1)

let length s =
  Uutf.String.fold_utf_8 (fun accum _ c -> match c with `Malformed _ -> accum | `Uchar _ -> succ accum) 0 s
