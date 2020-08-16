(** Implementation for the line of application information. *)
class t :
  unit
  -> object
       inherit LTerm_widget.hbox

       method set_filter_name : string -> unit
       (** [set_filter_name name] set name of filter to show *)

       method set_number_of_candidates : int -> unit
       (** [set_number_of_candidates number] set number of candidates. *)
     end
