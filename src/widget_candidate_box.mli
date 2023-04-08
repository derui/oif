type action =
  | Next_candidate
  | Prev_candidate
  | Toggle_mark
  | Confirm_candidate

type event = Confirmed of int list

(** Implementation for the box to show candidate and navigate. *)
class t :
  Index_coordinator.t
  -> object
       inherit LTerm_widget.t

       method notify_candidates_updated : unit -> unit
       (** [notify_candidate_updated ()] is called by user of this widget to notify candidates updated *)

       method event : event React.E.t
       (** [event] get signal for event *)

       method bind : LTerm_key.t -> action -> unit
       (** [bind key action] bind an action to key. *)
     end
