open Oif_lib

type action =
  | Next_candidate
  | Prev_candidate
  | Toggle_mark
  | Confirm_candidate

(** Implementation for the box to show candidate and navigate. *)
class t :
  unit
  -> object
       inherit LTerm_widget.t

       method current_candidates : Candidate.id array React.signal
       (** Signal for current selected candidate or marked candidates *)

       method set_candidates : Candidate.t Vector.t ref -> unit
       (** [set_candidates signal] set candidates to show in this box. *)

       method set_matcher : Matcher.t -> unit
       (** [set_matcher data] set matcher to box. *)

       method bind : LTerm_key.t -> action -> unit
       (** [bind key action] bind an action to key. *)
     end
