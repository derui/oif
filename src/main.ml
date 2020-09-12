open Std

type exit_status =
  | Confirm              of string
  | Confirmed_with_empty
  | Quit

let name_of_filter = function Main_widget.Partial_match -> "Partial match" | Main_widget.Migemo -> "Migemo"

module App_state = struct
  type t = {
    mutable current_filter : (module Filter.S);
    available_filters : (module Filter.S) list;
  }

  let find_filter t name = List.find ~f:(fun (module F : Filter.S) -> F.unique_name = name) t.available_filters

  let change_filter t information_line = function
    | Main_widget.Partial_match as v ->
        let filter_name = name_of_filter v in
        information_line#set_filter_name filter_name;
        find_filter t filter_name |> Option.iter (fun filter -> t.current_filter <- filter)
    | Main_widget.Migemo as v        ->
        let filter_name = name_of_filter v in
        information_line#set_filter_name filter_name;
        find_filter t filter_name |> Option.iter (fun filter -> t.current_filter <- filter)
end

let open_tty fname = Lwt_unix.openfile fname [ Lwt_unix.O_RDWR ] 0o666

let load_lines channel =
  let open Lwt in
  catch
    (fun () ->
      let rec read_lines channel lines =
        catch
          (fun () -> Lwt_io.read_line channel >>= fun line -> read_lines channel (line :: lines))
          (fun _ -> return @@ List.rev lines)
      in
      read_lines channel [])
    (fun _ -> return [])

let process_when_tty () =
  let%lwt lines = load_lines Lwt_io.stdin in
  Lwt.return lines

let make_info lines = Lwt.return @@ Types.Info.init lines

let filter_candidate app_state info text =
  let module F = (val app_state.App_state.current_filter) in
  F.filter ~info ~text

(* event handlers *)

let selection_event_handler app_state box info text =
  let module F = (val app_state.App_state.current_filter) in
  let candidates = filter_candidate app_state info text in
  box#set_candidates candidates

let confirm_candidate_handler wakener candidate =
  match Option.map Types.Candidate.text candidate with
  | None   -> Lwt.wakeup_later wakener Confirmed_with_empty
  | Some v -> Lwt.wakeup_later wakener (Confirm v)

let change_filter_handler app_state filter = App_state.change_filter app_state filter

let quit_handler wakener = function false -> () | true -> Lwt.wakeup_later wakener Quit

let load_migemo_filter option =
  option.Cli_option.migemo_dict_directory
  |> Option.map (fun dict_dir ->
         Migemocaml.Migemo.make_from_dir ~spec:(module Migemocaml.Regexp_spec.OCaml_str) ~base_dir:dict_dir ())
  |> Option.join
  |> Option.map (fun migemo : (module Filter.S) ->
         ( module Filter.Migemo (struct
           let migemo = migemo
         end) ))
  |> Option.to_list

let create_window () =
  let%lwt tty_fd = open_tty "/dev/tty" in
  let in_chan = Lwt_io.of_fd ~mode:Lwt_io.input tty_fd in
  let out_chan = Lwt_io.of_fd ~mode:Lwt_io.output tty_fd in
  LTerm.create tty_fd in_chan tty_fd out_chan

let () =
  Cli_option.parse (fun option ->
      let app_state =
        {
          App_state.current_filter = (module Filter.Partial_match);
          available_filters = [ (module Filter.Partial_match : Filter.S) ] @ load_migemo_filter option;
        }
      in
      let monad =
        let%lwt info = Lwt.(process_when_tty () >>= make_info) in
        let%lwt window = create_window () in
        let box = new Widget_candidate_box.t () in
        let information_line = new Widget_information_line.t () in
        let read_line = new Widget_read_line.t ?query:option.query () in
        let term =
          new Main_widget.t
            ~box:(box :> LTerm_widget.t)
            ~read_line:(read_line :> LTerm_widget.t)
            ~information_line:(information_line :> LTerm_widget.t)
            ()
        in
        let candidates =
          Types.Info.to_candidates info |> fun candidates ->
          match option.query with None -> candidates | Some v -> filter_candidate app_state info v
        in
        box#set_candidates candidates;
        information_line#set_number_of_candidates @@ List.length candidates;
        information_line#set_filter_name @@ name_of_filter Main_widget.Partial_match;

        (* define event and handler *)
        let () =
          React.S.changes read_line#text
          |> React.E.map (fun text -> selection_event_handler app_state box info text)
          |> Lwt_react.E.keep
        in
        let () =
          React.S.changes term#switch_filter
          |> React.E.map (change_filter_handler app_state information_line)
          |> Lwt_react.E.keep
        in
        let waiter, wakener = Lwt.task () in
        let () = React.S.changes term#quit |> React.E.map (quit_handler wakener) |> Lwt_react.E.keep in
        let () =
          React.S.changes box#current_candidate |> React.E.map (confirm_candidate_handler wakener) |> Lwt_react.E.keep
        in

        let%lwt mode = LTerm.enter_raw_mode window in
        try%lwt LTerm_widget.run window term waiter
        with _ ->
          let%lwt () = LTerm.leave_raw_mode window mode in
          Lwt.return Quit
      in
      let result = Lwt_main.run monad in
      match result with
      | Quit                 -> exit 130
      | Confirmed_with_empty -> exit 1
      | Confirm v            ->
          print_string v;
          exit 0)
