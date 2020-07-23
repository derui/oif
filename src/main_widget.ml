(** Implementation for main widget. *)
class t ~box ~read_line () =
  object (self)
    inherit LTerm_widget.vbox

    (* method! draw ctx _ =
     *   let size = LTerm_draw.size ctx in
     *   let read_line_rect = { LTerm_geom.row1 = 0; row2 = 1; col1 = 0; col2 = size.LTerm_geom.cols } in
     *   let read_line_ctx = LTerm_draw.sub ctx read_line_rect in
     *   let hline_ctx = LTerm_draw.sub ctx { LTerm_geom.row1 = 1; row2 = 2; col1 = 0; col2 = size.cols } in
     *   let box_ctx = LTerm_draw.sub ctx { LTerm_geom.row1 = 2; row2 = size.rows - 2; col1 = 0; col2 = size.cols } in
     *   let ctxs = [ read_line_ctx; hline_ctx; box_ctx ] in
     *
     *   List.iteri (fun i child -> child#draw (List.nth ctxs i) child) self#children *)
    initializer
    (* self#on_event (fun e ->
     *     box#send_event e;
     *     read_line#send_event e;
     *     true); *)
    self#add ~expand:false (new LTerm_widget.hline);
    self#add ~expand:false read_line;
    self#add ~expand:true box
    (* method register_event_handler term =
     *   let awaiter, awaker = Lwt.task () in
     *   let open Lwt in
     *   let rec event_loop () =
     *     let%lwt event = LTerm.read_event term in
     *     box#send_event event |> event_loop
     *   in
     *   (Lwt.choose [ awaiter; event_loop () ], Lwt.wakeup awaker) *)
  end
