type filter =
  | Partial_match
  | Migemo

(** Implementation for the box to show candidate and navigate. *)
class t :
  box:LTerm_widget.t
  -> read_line:LTerm_widget.t
  -> information_line:LTerm_widget.t
  -> event_hub:LTerm_event.t Event_hub.t
  -> unit
  -> object
       inherit LTerm_widget.vbox

       method switch_filter : filter React.signal

       method quit : bool React.signal
     end
