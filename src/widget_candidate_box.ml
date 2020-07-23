open CamomileLibrary
open Std
module VW = Oif_lib.Virtual_window
module Bindings = Zed_input.Make (LTerm_key)

let selection_prefix = "-> "

type action =
  | Next_candidate
  | Prev_candidate

(** Implementation for the box to show candidate and navigate. *)
class t () =
  let current_selection, _ = React.S.create 0 in
  let candidates, set_candidates = React.S.create [||] in
  object (self)
    inherit LTerm_widget.t "candidate_box"

    val mutable _virtual_window = VW.create ()

    method set_candidates candidates = set_candidates @@ Array.of_list candidates

    method current_candidate =
      React.S.l2 (fun candidates selection -> candidates.(selection)) candidates current_selection

    val mutable bindings : action Bindings.t = Bindings.empty

    method bind key action = bindings <- Bindings.add [ key ] action bindings

    method private draw_candidate ctx l candidate =
      let module C = Types.Candidate in
      let size = LTerm_draw.size ctx in
      let rect =
        { LTerm_geom.row1 = l; row2 = l + 1; col1 = String.length selection_prefix; col2 = LTerm_geom.cols size }
      in
      let ctx = LTerm_draw.sub ctx rect in
      let f index c styles =
        let style =
          List.find styles ~f:(fun (i, _) -> i = index) |> function None -> None | Some (_, style) -> Some style
        in
        LTerm_draw.draw_char ?style ctx 0 index (Zed_char.of_utf8 @@ Char.escaped @@ UChar.char_of c)
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
        VW.update_total_rows (Array.length candidates) _virtual_window |> VW.update_view_port_size size.rows;

      let w = VW.calculate_window _virtual_window in
      let len = VW.Window.(end_index w - start_index w) in
      Array.sub candidates VW.Window.(start_index w) len
      |> Array.to_list |> List.filter ~f:Option.is_some
      |> List.iteri (fun index candidate ->
             match candidate with None -> () | Some candidate -> self#draw_candidate ctx index candidate);

      if Array.length candidates > 0 then self#draw_selection ctx selection else ()

    method! draw ctx _ =
      let candidates = React.S.value candidates in
      Lwt_react.S.map_s (fun selection -> self#render ctx candidates selection |> Lwt.return) current_selection
      |> Lwt.ignore_result
  end
