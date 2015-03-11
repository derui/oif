open CamomileLibrary
open Core.Std

let draw_candidate ctx l candidate =
  let module C = Types.Candidate in
  let f index c styles =
    let style = List.find styles ~f:(fun (i, _) -> i = index) |> function
      | None -> None
      | Some (_, style) -> Some style
    in
    LTerm_draw.draw_char ?style ctx l index c
  in
  C.map_text ~f candidate |> ignore

let draw_selection ctx = function
  | None -> ()
  | Some (row, style) ->
     let size = LTerm_draw.size ctx in
     let rect = {LTerm_geom.row1 = row; row2 = row + 1; col1 = 0;col2 = LTerm_geom.cols size} in
     let ctx = LTerm_draw.sub ctx rect in
     LTerm_draw.fill_style ctx style

let render window candidates selection =
  let open Lwt in
  LTerm.goto window {LTerm_geom.row = 1; col = 0} >>= fun () ->
  let size = LTerm.size window in
  (* This reassignment size to avoid problem what read_line can not raise event inputting text. *)
  let size = {LTerm_geom.rows = LTerm_geom.rows size;
              cols = LTerm_geom.cols size} in
  let mat = LTerm_draw.make_matrix size in
  let displayed = LTerm_draw.make_matrix {LTerm_geom.rows = 1;cols = LTerm_geom.cols size} in
  let rect = {LTerm_geom.row1 = 1;col1 = 0;
              row2 = LTerm_geom.rows size;col2 = LTerm_geom.cols size} in
  let ctx = LTerm_draw.context mat size in
  let ctx = LTerm_draw.sub ctx rect in
  List.fold candidates ~init:0 ~f:(fun current candidate ->
    match candidate with
    | None -> current
    | Some candidate -> begin
      draw_candidate ctx current candidate;
      succ current
    end
  ) |> ignore;

  draw_selection ctx selection;

  LTerm.render_update window displayed mat >>= fun () ->
  LTerm.goto window {LTerm_geom.row = 0; col = 0}
