open CamomileLibraryDefault.Camomile
open Oif_lib
module VW = Virtual_window
module Bindings = Zed_input.Make (LTerm_key)
module Array = Vector
module S = Candidate_selector

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
  let selector, set_selector = React.S.create ~eq:(fun _ _ -> false) @@ Candidate_selector.make () in
  let candidates, set_candidates = React.S.create ~eq:(fun _ _ -> false) @@ ref @@ Vector.empty () in
  let matcher, set_matcher =
    React.S.create ~eq:(fun _ _ -> false) @@ Matcher.make ~candidates:(ref @@ Vector.empty ())
  in
  let current_candidates, set_current_candidates = React.S.create ~eq:(fun _ _ -> false) [||] in
  let item_marker, set_item_marker = React.S.create ~eq:Item_marker.equal Item_marker.empty in

  let view_port_size ctx =
    let size = LTerm_draw.size ctx in
    size.rows
  in

  object (self)
    inherit LTerm_widget.t "oif:candidate_box"

    val mutable _virtual_window = VW.create ()

    method set_candidates candidates' =
      set_candidates candidates';
      let new_candidate_size = Array.length !candidates' in
      React.S.value selector |> S.restrict_with_limit ~limit:new_candidate_size |> set_selector

    method set_matcher v = set_matcher v

    method current_candidates = current_candidates

    val mutable bindings : action Bindings.t = Bindings.empty

    method bind key action = bindings <- Bindings.add [ key ] action bindings

    method private draw_candidate ctx l selected candidate ~marked ~result =
      let module C = Candidate_style in
      let size = LTerm_draw.size ctx in
      let rect = { LTerm_geom.row1 = l; row2 = l + 1; col1 = prefix_length; col2 = LTerm_geom.cols size } in
      let ctx' = LTerm_draw.sub ctx rect in
      let text = C.make_styled_candidate ~selected ~result candidate in
      LTerm_draw.draw_styled ctx' 0 0 text;

      if marked then
        let col1 = String.length selection_prefix and col2 = prefix_length in
        let rect = { LTerm_geom.row1 = l; row2 = l + 1; col1; col2 } in
        let ctx' = LTerm_draw.sub ctx rect in
        Zed_string.of_utf8 marked_prefix
        |> LTerm_draw.draw_string ctx' 0 0 ~style:{ LTerm_style.none with foreground = Some LTerm_style.lcyan }
      else ()

    method private draw_selection ctx row =
      let size = LTerm_draw.size ctx in
      let row = min row @@ pred size.rows in
      let rect = { LTerm_geom.row1 = row; row2 = row + 1; col1 = 0; col2 = String.length selection_prefix } in
      let ctx = LTerm_draw.sub ctx rect in
      Zed_string.of_utf8 selection_prefix
      |> LTerm_draw.draw_string ctx 0 0 ~style:{ LTerm_style.none with foreground = Some LTerm_style.lred }

    method private render ctx candidates selection item_marker matcher =
      let match_results = Matcher.match_results matcher and matched_indices = Matcher.matched_indices matcher in
      let view_port_height = view_port_size ctx in
      _virtual_window <-
        VW.update_total_rows (Array.length matched_indices) _virtual_window
        |> VW.update_view_port_size view_port_height
        |> VW.update_focused_row selection;

      let w = VW.calculate_window _virtual_window in
      let len = succ @@ VW.Window.(end_index w - start_index w) in
      if Array.length matched_indices <= 0 then ()
      else
        let start_index = VW.Window.(start_index w) in
        Array.sub matched_indices start_index len
        |> Array.iteri ~f:(fun index matched_index ->
               let candidate = Array.unsafe_get candidates matched_index
               and match_result =
                 if Array.length match_results < Array.length matched_indices then Match_result.empty
                 else Array.unsafe_get match_results matched_index
               in
               let marked = Item_marker.is_marked ~index:matched_index item_marker in
               self#draw_candidate ctx index (index = selection) candidate ~marked ~result:match_result);

        let selection = selection - start_index in
        if Array.length matched_indices > 0 then self#draw_selection ctx selection else ()

    method! draw ctx _ =
      let candidates = React.S.value candidates in
      let matcher = React.S.value matcher in
      let item_marker = React.S.value item_marker in
      let selection = React.S.value selector |> S.current_selected_index in
      self#render ctx !candidates selection item_marker matcher

    method private exec action =
      let matcher = React.S.value matcher in
      let matched_indices = Matcher.matched_indices matcher in
      let candidate_size = matched_indices |> Array.length in
      match action with
      | Next_candidate    -> set_selector @@ S.select_next ~indices:matched_indices @@ React.S.value selector
      | Prev_candidate    -> set_selector @@ S.select_previous @@ React.S.value selector
      | Confirm_candidate ->
          let selected_index = React.S.value selector |> S.current_selected_index
          and current_marker = React.S.value item_marker in

          if Item_marker.is_empty current_marker then
            set_current_candidates
              (if selected_index >= candidate_size then [||] else [| Array.unsafe_get matched_indices selected_index |])
          else
            let marked_indices = Item_marker.marked_indices current_marker |> Stdlib.Array.of_seq in
            set_current_candidates marked_indices
      | Toggle_mark       ->
          let selected_index = React.S.value selector |> S.current_selected_index
          and marker = React.S.value item_marker in
          let start_index = VW.calculate_window _virtual_window |> VW.Window.start_index in
          let candidate_index = start_index + selected_index in
          let candidate_index = Array.unsafe_get matched_indices candidate_index in
          if candidate_size > candidate_index then
            set_item_marker (Item_marker.toggle_mark ~index:candidate_index marker)
          else ()

    method private handle_event event =
      match event with
      | LTerm_event.Key key ->
          let resolver = Bindings.resolver [ Bindings.pack (fun x -> x) bindings ] in
          (match Bindings.resolve key resolver with Bindings.Accepted action -> self#exec action | _ -> ());
          true
      | _                   -> false

    val mutable _selection_update_event = React.E.never

    initializer
    (* keep event reference *)
    _selection_update_event <-
      React.E.select
        [
          React.E.stamp (React.S.changes selector) ignore;
          React.E.stamp (React.S.changes candidates) ignore;
          React.E.stamp (React.S.changes item_marker) ignore;
          React.E.stamp (React.S.changes matcher) ignore;
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
