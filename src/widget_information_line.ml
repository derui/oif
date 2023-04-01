let prefix = String.make 3 ' '

class label_num_of_candidates () =
  let number_of_candidates, set_number_of_candidates = React.S.create 0 in
  let size_request, set_size_request = React.S.create { LTerm_geom.rows = 1; cols = 0 } in
  object (self)
    inherit LTerm_widget.t "oif:information_line:candidates"

    method signal = number_of_candidates

    method set_number_of_candidates v = set_number_of_candidates v

    method private make_string v = Printf.sprintf "%s%d" prefix v

    method! size_request = React.S.value size_request

    method! draw ctx _ =
      let number_of_candidates = React.S.value number_of_candidates in
      let text = Zed_string.of_utf8 @@ self#make_string number_of_candidates in
      let style = { LTerm_style.none with foreground = Some LTerm_style.green } in
      LTerm_draw.draw_string ctx 0 0 ~style text

    val mutable _event_cache = React.E.never

    initializer
      _event_cache <-
        React.S.changes number_of_candidates
        |> React.E.map (fun v ->
               let s = self#make_string v in
               set_size_request { LTerm_geom.rows = 1; cols = String.length s })
        |> React.E.map (fun () -> self#queue_draw)
  end

class label_filter_name () =
  let filter_name, set_filter_name = React.S.create "" in
  let size_request, set_size_request = React.S.create { LTerm_geom.rows = 1; cols = 0 } in
  object (self)
    inherit LTerm_widget.t "oif:information_line:filter"

    method signal = filter_name

    method set_filter_name v = set_filter_name v

    method! size_request = React.S.value size_request

    method! draw ctx _ =
      let filter_name = React.S.value filter_name in
      let text = Zed_string.of_utf8 filter_name in
      let style = { LTerm_style.none with foreground = Some LTerm_style.lgreen } in
      LTerm_draw.draw_string ctx 0 0 ~style text

    val mutable _event_cache = React.E.never

    initializer
      _event_cache <-
        React.S.changes filter_name
        |> React.E.map (fun s -> set_size_request { LTerm_geom.rows = 0; cols = String.length s })
        |> React.E.map (fun () -> self#queue_draw)
  end

(** Implementation for the box to show candidate and navigate. *)
class t () =
  let label_num = new label_num_of_candidates () and label_filter = new label_filter_name () in
  object (self)
    inherit LTerm_widget.hbox

    method set_number_of_candidates v = label_num#set_number_of_candidates v

    method set_filter_name v = label_filter#set_filter_name v

    val mutable _selection_update_event = React.E.create () |> fst

    initializer
      (* keep event reference *)
      _selection_update_event <-
        React.E.select
          [
            React.E.stamp (React.S.changes label_num#signal) ignore;
            React.E.stamp (React.S.changes label_filter#signal) ignore;
          ]
        |> React.E.map (fun _ -> self#queue_draw);

      self#add ~expand:true label_num;
      self#add ~expand:false label_filter
  end
