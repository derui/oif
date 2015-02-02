open CamomileLibrary
open Core.Std

class read_line ~term ~history ~exit_code =
  let text_s, set_text = React.E.create () in
  let selection_s, set_selection = React.E.create () in
  let selection = ref 0 in
object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term as super

  method show_box = false

  method text = text_s
  method private set_text = set_text

  (* Current selection. It is origin from 0. *)
  method selection = React.E.changes selection_s
  method private set_selection = set_selection

  method current_selection () = !selection

  method private exec actions =
    let open Lwt in
    match actions with
    | [] -> super#exec []
    | action :: rest -> begin match action with
      | LTerm_read_line.Accept -> return (Zed_edit.text self#edit |> Zed_rope.to_string)
      | LTerm_read_line.Complete -> return (Zed_edit.text self#edit |> Zed_rope.to_string)
      | LTerm_read_line.Complete_bar_next -> selection := succ !selection;
        self#set_selection !selection;
        self#exec rest
      | LTerm_read_line.Complete_bar_prev -> selection := pred !selection |> max 0;
        self#set_selection !selection;
        self#exec rest
      | _ -> begin self#send_action action; self#exec rest end
    end

  initializer
  let signal = LTerm_text.of_string "QUERY> " |> React.S.const in
  self#set_prompt signal;
  React.E.map (fun _ ->
    let text = Zed_edit.text self#edit |> Zed_rope.to_string  in
    self#set_text text
  ) (Zed_edit.changes self#edit) |> ignore;
  self#set_selection 0;

  Zed_macro.add self#macro LTerm_read_line.Accept;

end
