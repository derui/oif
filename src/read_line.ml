open CamomileLibrary
open Core.Std

class read_line ~term ~history ~exit_code =
  let selection_s, set_selection = React.S.create ~eq:(fun _ _ -> false) (None : Types.direction option) in
object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term as super

  method show_box = false

  (* Current selection. It is origin from 0. *)
  method selection = selection_s
  method private set_selection = set_selection

  method current_selection () = React.S.value selection_s

  method private exec actions =
    let open Lwt in
    match actions with
    | [] -> super#exec []
    | action :: rest -> begin match action with
      | LTerm_read_line.Accept -> return (Zed_edit.text self#edit |> Zed_rope.to_string)
      | LTerm_read_line.Complete -> return (Zed_edit.text self#edit |> Zed_rope.to_string)
      | LTerm_read_line.Complete_bar_next ->
         self#set_selection (Some Types.Next);
         super#exec []
      | LTerm_read_line.Complete_bar_prev -> 
         self#set_selection (Some Types.Prev); 
         super#exec []
      | _ -> begin self#send_action action; self#exec rest end
    end

  initializer
  let signal = LTerm_text.of_string "QUERY> " |> React.S.const in
  self#set_prompt signal;

  Zed_macro.add self#macro LTerm_read_line.Accept;

end
