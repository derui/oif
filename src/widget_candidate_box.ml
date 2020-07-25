open CamomileLibraryDefault.Camomile
open Std
module VW = Oif_lib.Virtual_window
module Bindings = Zed_input.Make (LTerm_key)

let selection_prefix = "-> "

type action =
  | Next_candidate
  | Prev_candidate
  | Confirm_candidate

(** Implementation for the box to show candidate and navigate. *)
class t () =
  let current_selection, set_selection = React.S.create 0 in
  let candidates, set_candidates = React.S.create [||] in
  let current_candidate, set_current_candidate = React.S.create ~eq:(fun _ _ -> false) None in
  object (self)
    inherit LTerm_widget.t "candidate_box"

    val mutable _virtual_window = VW.create ()

    method set_candidates candidates = set_candidates @@ Array.of_list candidates

    method current_candidate = current_candidate

    val mutable bindings : action Bindings.t = Bindings.empty

    method bind key action = bindings <- Bindings.add [ key ] action bindings

    method private draw_candidate ctx l selected candidate =
      let module C = Types.Candidate in
      let size = LTerm_draw.size ctx in
      let rect =
        { LTerm_geom.row1 = l; row2 = l + 1; col1 = String.length selection_prefix; col2 = LTerm_geom.cols size }
      in
      let ctx = LTerm_draw.sub ctx rect in
      let f index c styles =
        let style =
          List.find styles ~f:(fun (i, _) -> i = index) |> function
          | None            -> if selected then { LTerm_style.none with bold = Some true } else LTerm_style.none
          | Some (_, style) -> if selected then { style with bold = Some true } else style
        in
        LTerm_draw.draw_char ~style ctx 0 index (Zed_char.of_utf8 @@ Char.escaped @@ UChar.char_of c)
      in
      C.iter_text ~f candidate |> ignore

    method private draw_selection ctx row =
      let rect = { LTerm_geom.row1 = row; row2 = row + 1; col1 = 0; col2 = String.length selection_prefix } in
      let ctx = LTerm_draw.sub ctx rect in
      Zed_string.of_utf8 selection_prefix
      |> LTerm_draw.draw_string ctx 0 0 ~style:{ LTerm_style.none with foreground = Some LTerm_style.lred }

    method private render ctx candidates selection =
      let size = LTerm_draw.size ctx in
      _virtual_window <-
        VW.update_total_rows (Array.length candidates) _virtual_window
        |> VW.update_view_port_size size.rows |> VW.update_focused_row selection;

      let w = VW.calculate_window _virtual_window in
      let len = succ @@ VW.Window.(end_index w - start_index w) in
      if Array.length candidates <= 0 then ()
      else
        Array.sub candidates VW.Window.(start_index w) len
        |> Array.iteri (fun index candidate -> self#draw_candidate ctx index (index = selection) candidate);

      if Array.length candidates > 0 then self#draw_selection ctx selection else ()

    method! draw ctx _ =
      let candidates = React.S.value candidates in
      let current_selection = React.S.value current_selection in
      self#render ctx candidates current_selection

    method private exec action =
      let candidate_size = React.S.value candidates |> Array.length |> pred in
      match action with
      | Next_candidate    ->
          if candidate_size <= 0 then ()
          else set_selection (min candidate_size @@ (React.S.value current_selection |> succ))
      | Prev_candidate    ->
          if candidate_size <= 0 then () else set_selection (max 0 @@ (React.S.value current_selection |> pred))
      | Confirm_candidate ->
          let candidates = React.S.value candidates and selection = React.S.value current_selection in
          set_current_candidate (if selection >= Array.length candidates then None else Some candidates.(selection))

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
        [ React.E.stamp (React.S.changes current_selection) ignore; React.E.stamp (React.S.changes candidates) ignore ]
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
      { control = false; meta = false; shift = false; code = Enter })
      Confirm_candidate
  end
