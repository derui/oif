open CamomileLibrary
open Core.Std

class read_line ~term ~history ~exit_code =
  let text_s, set_text = React.E.create () in
object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term as super

  method show_box = false

  method text = text_s
  method set_text = set_text

  method private exec actions =
    let open Lwt in
    match actions with
    | [] -> super#exec actions
    | action :: rest -> begin match action with
      | LTerm_read_line.Accept -> return (Zed_edit.text self#edit |> Zed_rope.to_string)
      | _ -> begin self#send_action action; self#exec rest end
    end

  initializer
  let signal = LTerm_text.of_string "QUERY> " |> React.S.const in
  self#set_prompt signal;
  React.E.map (fun _ ->
    let text = Zed_edit.text self#edit |> Zed_rope.to_string  in
    self#set_text text
  ) (Zed_edit.changes self#edit) |> ignore;
  Zed_macro.add self#macro LTerm_read_line.Accept;

end
