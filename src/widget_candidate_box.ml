open CamomileLibraryDefault.Camomile
open Std
module VW = Oif_lib.Virtual_window
module Bindings = Zed_input.Make (LTerm_key)

let selection_prefix = "->"

let marked_prefix = ">"

let prefix_length = String.length selection_prefix + String.length marked_prefix

type action =
  | Next_candidate
  | Prev_candidate
  | Toggle_mark
  | Confirm_candidate

(** Implementation for the box to show candidate and navigate. *)
class t () =
  let current_selection, set_selection = React.S.create 0 in
  let candidates, set_candidates = React.S.create [||] in
  let current_candidates, set_current_candidates = React.S.create [] in
  let item_marker, set_item_marker = React.S.create ~eq:Types.Item_marker.equal Types.Item_marker.empty in

  object (self)
    inherit LTerm_widget.t "oif:candidate_box"

    val mutable _virtual_window = VW.create ()

    method set_candidates candidates =
      let new_candidate_size = List.length candidates in
      set_candidates @@ Array.of_list candidates;
      let selection = React.S.value current_selection in
      if new_candidate_size <= selection then set_selection (max 0 @@ min (pred new_candidate_size) selection) else ()

    method current_candidates = current_candidates

    val mutable bindings : action Bindings.t = Bindings.empty

    method bind key action = bindings <- Bindings.add [ key ] action bindings

    method private draw_candidate ctx l selected candidate ~marked =
      let module C = Types.Candidate in
      let size = LTerm_draw.size ctx in
      let rect = { LTerm_geom.row1 = l; row2 = l + 1; col1 = prefix_length; col2 = LTerm_geom.cols size } in
      let ctx' = LTerm_draw.sub ctx rect in
      let text = C.make_styled_text selected candidate in
      LTerm_draw.draw_styled ctx' 0 0 text;

      if marked then
        let col1 = String.length selection_prefix and col2 = prefix_length in
        let rect = { LTerm_geom.row1 = l; row2 = l + 1; col1; col2 } in
        let ctx' = LTerm_draw.sub ctx rect in
        Zed_string.of_utf8 marked_prefix
        |> LTerm_draw.draw_string ctx' 0 0 ~style:{ LTerm_style.none with foreground = Some LTerm_style.lcyan }
      else ()

    method private draw_selection ctx row =
      let rect = { LTerm_geom.row1 = row; row2 = row + 1; col1 = 0; col2 = String.length selection_prefix } in
      let ctx = LTerm_draw.sub ctx rect in
      Zed_string.of_utf8 selection_prefix
      |> LTerm_draw.draw_string ctx 0 0 ~style:{ LTerm_style.none with foreground = Some LTerm_style.lred }

    method private render ctx candidates selection item_marker =
      let size = LTerm_draw.size ctx in
      _virtual_window <-
        VW.update_total_rows (Array.length candidates) _virtual_window
        |> VW.update_view_port_size size.rows |> VW.update_focused_row selection;

      let w = VW.calculate_window _virtual_window in
      let len = succ @@ VW.Window.(end_index w - start_index w) in
      if Array.length candidates <= 0 then ()
      else
        let start_index = VW.Window.(start_index w) in
        Array.sub candidates start_index len
        |> Array.iteri (fun index candidate ->
               let marked = Types.Item_marker.is_marked candidate item_marker in
               self#draw_candidate ctx index (index = selection) candidate ~marked);

        if Array.length candidates > 0 then self#draw_selection ctx selection else ()

    method! draw ctx _ =
      let candidates = React.S.value candidates in
      let current_selection = React.S.value current_selection in
      let item_marker = React.S.value item_marker in
      self#render ctx candidates current_selection item_marker

    method private exec action =
      let candidate_size = React.S.value candidates |> Array.length |> pred in
      match action with
      | Next_candidate    ->
          if candidate_size <= 0 then ()
          else set_selection (min candidate_size @@ (React.S.value current_selection |> succ))
      | Prev_candidate    ->
          if candidate_size <= 0 then () else set_selection (max 0 @@ (React.S.value current_selection |> pred))
      | Confirm_candidate ->
          let candidates = React.S.value candidates
          and selection = React.S.value current_selection
          and current_marker = React.S.value item_marker in

          if Types.Item_marker.is_empty current_marker then
            set_current_candidates (if selection >= Array.length candidates then [] else [ candidates.(selection) ])
          else
            let marked_candidates =
              candidates
              |> Array.map (fun l -> if Types.Item_marker.is_marked l current_marker then Some l else None)
              |> Array.map (fun v -> Option.to_list v)
              |> Array.to_seq |> List.of_seq |> List.flatten
            in
            set_current_candidates marked_candidates
      | Toggle_mark       ->
          let candidates = React.S.value candidates
          and selection = React.S.value current_selection
          and current_marker = React.S.value item_marker in
          let start_index = VW.calculate_window _virtual_window |> VW.Window.start_index in
          let candidate_index = start_index + selection in
          if Array.length candidates > candidate_index then
            set_item_marker (Types.Item_marker.toggle_mark candidates.(candidate_index) current_marker)
          else ()

    method private handle_event event =
      match event with
      | LTerm_event.Key key ->
          let resolver = Bindings.resolver [ Bindings.pack (fun x -> x) bindings ] in
          (match Bindings.resolve key resolver with Bindings.Accepted action -> self#exec action | _ -> ());
          true
      | _                   -> true

    val mutable _selection_update_event = React.E.create () |> fst

    initializer
    (* keep event reference *)
    _selection_update_event <-
      React.E.select
        [
          React.E.stamp (React.S.changes current_selection) ignore;
          React.E.stamp (React.S.changes candidates) ignore;
          React.E.stamp (React.S.changes item_marker) ignore;
        ]
      |> React.E.map (fun _ -> self#queue_draw);
    self#on_event self#handle_event;

    self#bind
      (let open LTerm_key in
      { control = true; meta = false; shift = false; code = Char (UChar.of_char 'n') })
      Next_candidate;
    self#bind
      (let open LTerm_key in
      { control = true; meta = false; shift = false; code = Char (UChar.of_char 'p') })
      Prev_candidate;
    self#bind
      (let open LTerm_key in
      { control = false; meta = false; shift = false; code = Tab })
      Toggle_mark;
    self#bind
      (let open LTerm_key in
      { control = false; meta = false; shift = false; code = Enter })
      Confirm_candidate
  end
