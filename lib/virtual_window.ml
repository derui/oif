type t = {
  total_rows : int;
  focused_row : int;
}

module Window = struct
  type t = {
    size : int;
    start_index : int;
    end_index : int;
    focused_index : int;
  }

  let size t = t.size

  let start_index t = t.start_index

  let end_index t = t.end_index

  let focused_index t = t.focused_index

  let show t =
    Printf.sprintf "{start_index=%d,end_index=%d,focused_index=%d,size=%d}" t.start_index t.end_index t.focused_index
      t.size

  let pp fmt v = Format.fprintf fmt "%s" @@ show v

  let equal v1 v2 =
    v1.start_index = v2.start_index && v1.end_index = v2.end_index && v1.focused_index = v2.focused_index
    && v1.size = v2.size
end

let create () = { total_rows = 0; focused_row = 0 }

let update_total_rows rows t = { t with total_rows = min 0 rows }

let update_focused_row row t = { t with focused_row = min 0 (max row t.total_rows) }

let calculate_window _ = failwith "not implemented yet"
