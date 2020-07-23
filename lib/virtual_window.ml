type start_index = int

type end_index = int

type t = {
  total_rows : int;
  focused_row : int;
  view_port : start_index * end_index;
}

let show t =
  Printf.sprintf "{total_rows=%d, focused_row=%d, view_port=(%d, %d)}" t.total_rows t.focused_row (fst t.view_port)
    (snd t.view_port)

module Window = struct
  type t = {
    size : int;
    start_index : start_index;
    end_index : end_index;
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

let create () = { total_rows = 0; focused_row = 0; view_port = (0, 0) }

let view_port_size t = max 0 @@ succ (snd t.view_port - fst t.view_port)

let update_total_rows rows t =
  assert (rows >= 0);
  let view_port_size = view_port_size t in
  let view_port = if view_port_size < rows then t.view_port else (0, pred rows) in
  let focused_row = if t.focused_row <= pred rows then t.focused_row else pred rows in
  let t = { total_rows = max 0 rows; view_port; focused_row } in
  t

let update_focused_row row t =
  assert (row >= 0);
  let focused_row = max 0 @@ min (pred t.total_rows) row in
  let start_index, end_index = t.view_port in
  let view_port_size = view_port_size t in
  let view_port =
    if focused_row < start_index then (focused_row, focused_row + pred view_port_size)
    else if focused_row > end_index then (focused_row - pred view_port_size, focused_row)
    else (start_index, end_index)
  in
  let t = { t with view_port; focused_row } in
  t

let update_view_port_size size t =
  assert (size >= 0);
  let start_index, _ = t.view_port in
  let view_port_size = view_port_size t in
  let view_port =
    if view_port_size >= size then (start_index, start_index + (max 0 @@ pred size))
    else if size >= t.total_rows then (0, max 0 @@ pred t.total_rows)
    else if start_index + size > pred t.total_rows then (pred (t.total_rows - size), max 0 @@ pred t.total_rows)
    else (start_index, start_index + (max 0 @@ pred size))
  in
  let focused_row = max (fst view_port) @@ min (snd view_port) t.focused_row in
  let t = { t with view_port; focused_row } in
  t

let calculate_window { total_rows; focused_row; view_port } =
  let start_index, end_index = view_port in
  let view_port_size = succ (end_index - start_index) in
  let size = if total_rows <= view_port_size then total_rows else view_port_size in

  { Window.size; start_index = fst view_port; end_index = snd view_port; focused_index = focused_row }
