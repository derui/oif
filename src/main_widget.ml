(** Implementation for main widget. *)
class t ~box () =
  object
    inherit LTerm_widget.t "main_widget"

    method! children = [ new LTerm_widget.hline; box ]
  end
