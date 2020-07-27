open Std

module App_state = struct
  type t = {
    mutable current_filter : (module Filter.S);
    available_filters : (module Filter.S) list;
  }

  let find_filter t name = List.find ~f:(fun (module F : Filter.S) -> F.unique_name = name) t.available_filters

  let change_filter t = function
    | Main_widget.Partial_match ->
        let filter_name = "Partial match" in
        find_filter t filter_name |> Option.iter (fun filter -> t.current_filter <- filter)
    | Main_widget.Migemo        ->
        let filter_name = "Migemo" in
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

let selection_event_handler app_state box info text =
  let module F = (val app_state.App_state.current_filter) in
  let candidates = F.filter ~info ~text in
  box#set_candidates candidates

let confirm_candidate_handler wakener candidate = Lwt.wakeup wakener @@ Option.map Types.Candidate.text candidate

let change_filter_handler app_state filter = App_state.change_filter app_state filter

let load_migemo_filter option =
  option.Cli_option.migemo_dict_directory
  |> Option.map (fun dict_dir ->
         Migemocaml.Migemo.make_from_dir ~spec:(module Migemocaml.Regexp_spec.OCaml_str) ~base_dir:dict_dir ())
  |> Option.join
  |> Option.map (fun migemo ->
         ( module Filter.Migemo (struct
           let migemo = migemo
         end) : Filter.S ))
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
        let read_line = new Widget_read_line.t () in
        let term = new Main_widget.t ~box:(box :> LTerm_widget.t) ~read_line:(read_line :> LTerm_widget.t) () in
        let%lwt window_size = LTerm.get_size window in
        box#set_candidates @@ Types.Info.to_candidates info;
        LTerm.render window (LTerm_draw.make_matrix window_size);%lwt
        LTerm.goto window { LTerm_geom.row = 0; col = 0 };%lwt

        let () =
          React.S.changes term#switch_filter |> React.E.map (change_filter_handler app_state) |> Lwt_react.E.keep
        in

        (* define event and handler *)
        let () =
          React.S.changes read_line#text
          |> React.E.map (fun text -> selection_event_handler app_state box info text)
          |> Lwt_react.E.keep
        in
        let waiter, wakener = Lwt.task () in
        let () =
          React.S.changes box#current_candidate |> React.E.map (confirm_candidate_handler wakener) |> Lwt_react.E.keep
        in

        LTerm_widget.run window term waiter
      in
      let result = Lwt_main.run monad in
      match result with
      | None   -> exit 1
      | Some v ->
          print_string v;
          exit 0)
