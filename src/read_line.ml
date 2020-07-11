open CamomileLibrary

class read_line ~term ~history:_ ~exit_code:_ _ =
  let selection_s, set_selection = React.S.create ~eq:(fun _ _ -> false) (None : Types.direction option) in
  object (self)
    inherit LTerm_read_line.read_line ()

    inherit [Zed_string.t] LTerm_read_line.term term

    (* Current selection. It is origin from 0. *)
    method selection = selection_s

    method private set_selection = set_selection

    method current_selection () = React.S.value selection_s

    initializer
    let signal = Zed_string.of_utf8 "QUERY> " |> LTerm_text.of_string |> React.S.const in
    self#set_prompt signal;

    Zed_macro.add self#macro LTerm_read_line.Accept;
    let module K = LTerm_key in
    LTerm_read_line.bind
      [ { LTerm_key.code = LTerm_key.Char (UChar.of_char 'n'); control = true; meta = false; shift = false } ]
      [ LTerm_read_line.Complete_bar_next ];
    LTerm_read_line.bind
      [ { LTerm_key.code = LTerm_key.Char (UChar.of_char 'p'); control = true; meta = false; shift = false } ]
      [ LTerm_read_line.Complete_bar_prev ];
    LTerm_read_line.bind
      [ { K.control = true; meta = false; shift = false; code = K.Char (UChar.of_char ' ') } ]
      [ LTerm_read_line.Complete ]
  end
