open CamomileLibraryDefault.Camomile

type prompt = LTerm_text.t

type text = string

class label text () =
  object
    inherit LTerm_widget.t "oif:read_line:label"

    val mutable _text = text

    method text = text

    method set_text v = _text <- v

    method! size_request = LTerm_geom.{ rows = 1; cols = String.length _text }

    method! draw ctx _ =
      let text = LTerm_text.of_utf8 text in
      let style = { LTerm_style.none with bold = Some true } in
      Array.iteri (fun col (c, _) -> LTerm_draw.draw_char ctx 0 col ~style c) text
  end

class text_widget () =
  object
    inherit LTerm_edit.edit () as super

    method! draw ctx _focused =
      super#draw ctx _focused;
      match super#cursor_position with
      | None              -> ()
      | Some { col; row } ->
          let ctx = LTerm_draw.sub ctx { LTerm_geom.row1 = row; row2 = row + 1; col1 = col; col2 = col + 1 } in
          LTerm_draw.fill_style ctx LTerm_style.{ none with reverse = Some true }
  end

class t ?(prompt = "QUERY> ") ?(query = "") () =
  let text_widget = new text_widget () in

  let text_s, set_text = React.S.create "" in
  let prompt_s, set_prompt = LTerm_text.of_utf8 "" |> React.S.create in
  object (self)
    inherit LTerm_widget.hbox

    method text = text_s

    method prompt = prompt_s

    method set_prompt v = set_prompt v

    val mutable _text_event = React.E.create () |> fst

    initializer
    Zed_edit.insert text_widget#context @@ Zed_rope.of_string @@ Zed_string.of_utf8 query;

    let signal = Zed_string.of_utf8 prompt |> LTerm_text.of_string in
    self#set_prompt signal;
    _text_event <-
      React.E.select [ React.E.map ignore @@ Zed_edit.changes text_widget#engine ]
      |> React.E.map (fun _ -> set_text @@ Zed_string.to_utf8 text_widget#text);

    self#on_event (fun e ->
        text_widget#send_event e;
        true);

    LTerm_edit.unbind
      (let open LTerm_key in
      [ { control = true; meta = false; shift = false; code = Char (UChar.of_char 'p') } ]);
    LTerm_edit.unbind
      (let open LTerm_key in
      [ { control = true; meta = false; shift = false; code = Char (UChar.of_char 'n') } ]);
    LTerm_edit.unbind
      (let open LTerm_key in
      [ { control = false; meta = false; shift = false; code = Enter } ]);

    self#add ~expand:false (new label prompt ());
    self#add text_widget
  end
