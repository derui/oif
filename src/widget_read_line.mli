type prompt = LTerm_text.t

type text = string

class t :
  ?prompt:string
  -> ?query:string
  -> unit
  -> object
       inherit LTerm_widget.hbox

       method prompt : prompt React.signal

       method set_prompt : prompt -> unit
       (** Set new prompt *)

       method text : text React.signal
       (** Current inputted text *)
     end
