open Oif_lib
open Std
open Oif

type exit_status =
  | Confirm of string list
  | Confirmed_with_empty
  | Quit

let open_tty fname = Lwt_unix.openfile fname [ Lwt_unix.O_RDWR ] 0o666

let load_migemo_filter option cb =
  let open Option.Let_syntax in
  let v =
    let* dict_dir = option.Cli_option.migemo_dict_directory in
    let* migemo =
      Migemocaml.Migemo.make_from_dir ~spec:(module Migemocaml.Regexp_spec.OCaml_str) ~base_dir:dict_dir ()
    in
    let module V = Migemo_filter.Make (struct
      let migemo = migemo
    end) in
    Some (module V : Filter.S)
  in
  Option.to_list v |> List.map ~f:(fun m -> (Widget_main.Migemo, m)) |> cb

let get_tty_name () =
  let dev_prefixes = [ "/dev/pts/"; "/dev/" ] in
  let open Option.Let_syntax in
  let* stderr_stat = try Unix.fstat Unix.stderr |> Option.some with _ -> None in

  let rec loop = function
    | [] -> None
    | prefix :: rest -> (
        let files = Sys.readdir prefix in
        let ret =
          Array.to_list files
          |> List.find ~f:(fun file ->
                 try
                   let stat = Unix.stat @@ Filename.concat prefix file in
                   stat.st_rdev = stderr_stat.st_rdev
                 with _ -> false)
        in
        match ret with None -> loop rest | Some ret -> Filename.concat prefix ret |> Option.some)
  in
  loop dev_prefixes

let create_window () =
  match get_tty_name () with
  | Some tty ->
      let%lwt tty_fd = open_tty tty in
      let in_chan = Lwt_io.of_fd ~mode:Lwt_io.input tty_fd in
      let out_chan = Lwt_io.of_fd ~mode:Lwt_io.output tty_fd in
      LTerm.create tty_fd in_chan tty_fd out_chan
  | None -> LTerm.stdout |> Lazy.force

(* event handlers *)

let selection_event_handler app_state query = App_state.update_query query app_state

let confirm_candidate_handler wakener app_state = function
  | Widget_candidate_box.Confirmed selected_indices ->
      let candidates = App_state.matched_results app_state in
      let _ =
        match selected_indices with
        | [] -> Lwt.wakeup_later wakener Confirmed_with_empty
        | _ ->
            let ret = ref [] in
            List.iter
              ~f:(fun index -> ret := (Vector.unsafe_get candidates index |> fst |> Candidate.text) :: !ret)
              selected_indices;
            Lwt.wakeup_later wakener (Confirm !ret)
      in
      Lwt.return_unit

let change_filter_handler ~before_change app_state information_line filter =
  let%lwt () = before_change app_state in
  let%lwt () = App_state.change_filter app_state filter in
  let%lwt filter_name = App_state.current_filter_name app_state in
  information_line#set_filter_name filter_name |> Lwt.return

let quit_handler wakener = function false -> () | true -> Lwt.wakeup_later wakener Quit

type finalizer = unit -> unit

let finalizers : finalizer list ref = ref []

let add_finalizer finalizer = finalizers := finalizer :: !finalizers

let () =
  Cli_option.parse (fun option ->
      let app_state =
        App_state.make
          ~current_filter:(module Partial_match_filter)
          ~available_filters:[ (Widget_main.Partial_match, (module Partial_match_filter : Filter.S)) ]
      in

      let before_change app_state =
        let migemo_initialized = App_state.find_filter Widget_main.Migemo app_state |> Option.is_some in
        if migemo_initialized then Lwt.return_unit
        else
          load_migemo_filter option (fun f ->
              App_state.update_available_filters app_state (app_state.available_filters @ f) |> Lwt.return)
      in
      let async_reader = Async_line_reader.make () in

      let monad =
        let%lwt window = create_window () in

        let box = new Widget_candidate_box.t (Index_coordinator.make ~matcher:(fun () -> app_state.matcher)) () in
        let information_line = new Widget_information_line.t () in
        let read_line = new Widget_read_line.t ?query:option.query () in
        let module TR = Timestamp.Make (struct
          let now () = Unix.time () |> Int64.of_float
        end) in
        let hub = Event_hub.make (TR.make ()) in
        let term =
          new Widget_main.t
            ~box:(box :> LTerm_widget.t)
            ~read_line:(read_line :> LTerm_widget.t)
            ~information_line:(information_line :> LTerm_widget.t)
            ~event_hub:hub ()
        in
        information_line#set_filter_name @@ App_state.name_of_filter Widget_main.Partial_match;

        Option.iter
          (fun path ->
            let observer, finalizer = Event_recorder.init path in
            add_finalizer finalizer;
            Event_hub.add_observer observer hub |> ignore)
          option.Cli_option.record_event_path;

        Option.iter
          (fun path ->
            let replay = Event_replayer.init path hub in
            Lwt.async (fun () -> replay))
          option.Cli_option.replay_event_path;

        React.S.changes app_state.count_of_matches
        |> Lwt_react.E.map_s (fun number ->
               information_line#set_number_of_candidates number;
               box#notify_candidates_updated () |> Lwt.return)
        |> Lwt_react.E.keep;

        (* define event and handler *)
        React.S.changes read_line#text
        |> Lwt_react.E.map_s (fun text -> selection_event_handler app_state text)
        |> Lwt_react.E.keep;
        React.S.changes term#switch_filter
        |> Lwt_react.E.map_s (change_filter_handler app_state information_line ~before_change)
        |> Lwt_react.E.keep;
        let waiter, wakener = Lwt.task () in
        React.E.once term#quit |> React.E.map (quit_handler wakener) |> Lwt_react.E.keep;
        React.E.once box#event |> Lwt_react.E.map_s (confirm_candidate_handler wakener app_state) |> Lwt_react.E.keep;

        (* running asynchronous data reading *)
        let read_async, close_read_async = Async_line_reader.read_async Lwt_unix.stdin async_reader in
        let write_async = App_state.write_async (Async_line_reader.mailbox async_reader) app_state in
        add_finalizer close_read_async;
        Lwt.dont_wait (fun () -> read_async) ignore;
        Lwt.dont_wait (fun () -> write_async) ignore;

        let%lwt mode = LTerm.enter_raw_mode window in
        let%lwt status =
          try%lwt LTerm_widget.run window term waiter
          with _ ->
            Printexc.print_backtrace stdout;
            LTerm.leave_raw_mode window mode;%lwt
            Lwt.return Quit
        in
        List.iter ~f:(fun v -> v ()) !finalizers;
        Lwt.return status
      in
      match Lwt_main.run monad with
      | Quit -> exit 130
      | Confirmed_with_empty -> exit 1
      | Confirm v ->
          let delimiter = if option.print_nul then Char.chr 0 |> String.make 1 else "\n" in
          String.concat delimiter v |> Printf.printf "%s\n";
          exit 0)
