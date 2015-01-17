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

let render window candidates =
  let open Lwt in
  LTerm.goto window {LTerm_geom.row = 1; col = 0} >>= fun () ->
  let size = LTerm.size window in
  (* This reassignment size to avoid problem what read_line can not raise event inputting text. *)
  let size = {LTerm_geom.rows = max 0 (LTerm_geom.rows size |> pred);
              cols = LTerm_geom.cols size} in
  let mat = LTerm_draw.make_matrix size in
  let ctx = LTerm_draw.context mat size in
  List.fold candidates ~init:1 ~f:(fun current candidate ->
    draw_candidate ctx current candidate;
    succ current
  ) |> ignore;

  LTerm.render window mat >>= fun () ->
  LTerm.goto window {LTerm_geom.row = 0; col = 0}
