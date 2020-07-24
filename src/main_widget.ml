(** Implementation for main widget. *)
class t ~box ~read_line () =
  object (self)
    inherit LTerm_widget.vbox

    method! can_focus = true

    initializer
    self#on_event (fun e ->
        box#send_event e;
        read_line#send_event e;
        true);

    self#add ~expand:false (new LTerm_widget.hline);
    self#add ~expand:false read_line;
    self#add ~expand:true box
  end
