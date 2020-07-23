(** Implementation for main widget. *)
class t ~box ~read_line () =
  object (self)
    inherit LTerm_widget.vbox

    initializer
    self#add ~expand:false (new LTerm_widget.hline);
    self#add ~expand:false read_line;
    self#add ~expand:true box
  end
