class t ~term ~history:_ ~exit_code:_ _ =
  object (self)
    inherit LTerm_read_line.read_line ()

    inherit [Zed_string.t] LTerm_read_line.term term

    method! show_box = false

    initializer
    let signal = Zed_string.of_utf8 "QUERY> " |> LTerm_text.of_string |> React.S.const in
    self#set_prompt signal
  end
