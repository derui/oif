open Oif_lib
module VW = Virtual_window
module Bindings = Zed_input.Make (LTerm_key)
module Array = Vector
module IC = Index_coordinator

let selection_prefix = "->"

let marked_prefix = ">"

let prefix_length = String.length selection_prefix + String.length marked_prefix

type action =
  | Next_candidate
  | Prev_candidate
  | Toggle_mark
  | Confirm_candidate

type event = Confirmed of int list

let always_neq _ _ = false

(** Implementation for the box to show candidate and navigate. *)
class t (_coodinator : Index_coordinator.t) =
  let coordinator, set_coordinator = React.S.create ~eq:always_neq @@ _coodinator in
  let event', set_event = React.E.create () in

  let view_port_size ctx =
    let size = LTerm_draw.size ctx in
    size.rows
  in

  object (self)
    inherit LTerm_widget.t "oif:candidate_box"

    val mutable _virtual_window = VW.create ()

    val mutable bindings : action Bindings.t = Bindings.empty

    method event = event'

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

    method private update_virtual_window ctx total_size coordinator =
      let selection = IC.current_selected_index coordinator in
      let view_port_height = view_port_size ctx in
      _virtual_window <-
        VW.update_total_rows total_size _virtual_window
        |> VW.update_view_port_size view_port_height
        |> VW.update_focused_row selection

    method! draw ctx _ =
      let coordinator = React.S.value coordinator in
      let w = VW.calculate_window _virtual_window in

      let view_port_height = view_port_size ctx in
      IC.iter_with_matching coordinator ~size:view_port_height
        ~offset:VW.Window.(start_index w)
        ~f:(fun index matching ->
          self#draw_candidate ctx index matching.selected matching.candidate ~marked:matching.selected
            ~result:matching.match_result);
      self#update_virtual_window ctx view_port_height coordinator

    method private exec action =
      match action with
      | Next_candidate -> set_coordinator @@ IC.select_next @@ React.S.value coordinator
      | Prev_candidate -> set_coordinator @@ IC.select_previous @@ React.S.value coordinator
      | Confirm_candidate ->
          let v = IC.selected_indices @@ React.S.value coordinator in
          set_event (Confirmed v)
      | Toggle_mark ->
          let coordinator = React.S.value coordinator in
          set_coordinator @@ IC.toggle_mark_at_current_index coordinator

    method private handle_event event =
      match event with
      | LTerm_event.Key key ->
          let resolver = Bindings.resolver [ Bindings.pack (fun x -> x) bindings ] in
          (match Bindings.resolve key resolver with Bindings.Accepted action -> self#exec action | _ -> ());
          true
      | _ -> false

    val mutable _selection_update_event = React.E.never

    initializer
      let module Limiter = Lwt_throttle.Make (struct
        type t = string

        let equal = Stdlib.( = )

        let hash _ = 1
      end) in
      let limiter = Limiter.create ~max:1 ~n:1 ~rate:60 in
      let e = Lwt_react.E.map_s (fun _ -> Limiter.wait limiter "change") (React.S.changes coordinator) in
      (* keep event reference *)
      _selection_update_event <-
        React.E.select [ React.E.stamp (React.S.changes coordinator) ignore; React.E.stamp e ignore ]
        |> React.E.map (fun _ -> self#queue_draw);
      self#on_event self#handle_event;

      self#bind
        (let open LTerm_key in
         { control = true; meta = false; shift = false; code = Char (Uchar.of_char 'n') })
        Next_candidate;
      self#bind
        (let open LTerm_key in
         { control = true; meta = false; shift = false; code = Char (Uchar.of_char 'p') })
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
