type t = string

let string_to_uchar_list s =
  Uutf.String.fold_utf_8 (fun accum _ c -> match c with `Malformed _ -> accum | `Uchar c -> c :: accum) [] s
  |> List.rev

let index ~src ~part =
  let src = string_to_uchar_list src and part = string_to_uchar_list part in
  let not_found = -1 in

  let src_len = List.length src and part_len = List.length part in
  if src_len < part_len then not_found
  else if part_len = 0 then not_found
  else
    let origin_part = part in
    let rec find src part index matched_index =
      match (src, part) with
      | _, [] -> if matched_index <> -1 then matched_index else not_found
      | [], _ -> not_found
      | v1 :: r1, v2 :: r2 ->
          if Uchar.equal v1 v2 then find r1 r2 (succ index) (if matched_index <> -1 then matched_index else index)
          else find r1 origin_part (succ index) (-1)
    in
    find src part 0 (-1)

let length s =
  Uutf.String.fold_utf_8 (fun accum _ c -> match c with `Malformed _ -> accum | `Uchar _ -> succ accum) 0 s
