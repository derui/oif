(** Implementation for the box to show candidate and navigate. *)
class t :
  box:LTerm_widget.t
  -> unit
  -> object
       inherit LTerm_widget.t
     end
