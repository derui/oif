open CamomileLibrary
open Core.Std

let filter info text =
  let module I = Types.Info in
  let candidates = List.map info.I.lines ~f:(Text_match.decorate_to_match text) in
  info.I.candidates <- candidates;
  I.set_text info text

let update_selection info selection =
  let module I = Types.Info in
  let len = List.length info.I.candidates in
  if info.I.changed then
    info.I.selection <- None
  else begin
    match selection with
    | None -> info.I.selection <- None
    | Some dir -> match dir, info.I.selection with
      | Types.Next, None -> info.I.selection <- Some (0, Const.selection_style)
      | Types.Prev, None -> info.I.selection <- Some (0, Const.selection_style)
      | Types.Next, Some (row, _) ->
         info.I.selection <- Some (succ row |> min len, Const.selection_style)
      | Types.Prev, Some (row, _) ->
         info.I.selection <- Some (pred row |> max 0, Const.selection_style)
  end;
  info


