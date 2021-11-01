open CamomileLibraryDefault.Camomile

type filter =
  | Partial_match
  | Migemo

type action =
  | Change_filter
  | Quit

module Bindings = Zed_input.Make (LTerm_key)

(** Implementation for main widget. *)
class t ~box ~read_line ~information_line ~event_hub ~available_filters () =
  let switch_filter, set_switcn_filter = React.S.create Partial_match in
  let quit, set_quit = React.S.create false in

  let get_filter_to_switch = function
    | Partial_match -> List.find_opt (function Migemo -> true | _ -> false) available_filters
    | Migemo        -> List.find_opt (function Partial_match -> true | _ -> false) available_filters
  in
  object (self)
    inherit LTerm_widget.vbox

    val mutable bindings : action Bindings.t = Bindings.empty

    method switch_filter = switch_filter

    method quit = quit

    method private bind key action = bindings <- Bindings.add [ key ] action bindings

    method! can_focus = true

    method private exec =
      function
      | Change_filter -> (
          let current_filter = React.S.value switch_filter in
          match get_filter_to_switch current_filter with Some filter -> set_switcn_filter filter | None -> ())
      | Quit          -> set_quit true

    method private handle_event event =
      match event with
      | LTerm_event.Key key -> (
          let resolver = Bindings.resolver [ Bindings.pack (fun x -> x) bindings ] in
          match Bindings.resolve key resolver with
          | Bindings.Accepted action ->
              self#exec action;
              true
          | _                        -> false)
      | _                   -> false

    initializer
    let observer e =
      let e = Events.to_lterm_event e in
      if self#handle_event e then ()
      else (
        read_line#send_event e;
        box#send_event e)
    in
    Event_hub.add_observer observer event_hub |> ignore;

    self#on_event (fun e ->
        let event = Events.kind_of_lterm_event e in
        Option.iter (fun e -> Event_hub.dispatch e event_hub) event;
        true);

    self#add ~expand:false (new LTerm_widget.hline);
    self#add ~expand:false read_line;
    self#add ~expand:false information_line;
    self#add ~expand:false box;

    (* allocation initial size *)
    self#bind
      (let open LTerm_key in
      { control = true; meta = false; shift = false; code = Char (UChar.of_char 'q') })
      Change_filter;
    self#bind
      (let open LTerm_key in
      { control = true; meta = false; shift = false; code = Char (UChar.of_char 'c') })
      Quit;
    self#bind
      (let open LTerm_key in
      { control = true; meta = false; shift = false; code = Char (UChar.of_char 'g') })
      Quit
  end
