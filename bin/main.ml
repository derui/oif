open Oif_lib
open Std
open Oif

type exit_status =
  | Confirm              of string list
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
    | []             -> None
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
  | None     -> LTerm.stdout |> Lazy.force

(* event handlers *)

let selection_event_handler app_state box information_line query =
  App_state.update_query query app_state;
  let candidates = app_state.all_candidates in
  let number = App_state.count_of_matches app_state in
  information_line#set_number_of_candidates number;
  box#set_candidates candidates;
  box#set_matcher app_state.matcher |> Lwt.return

let confirm_candidate_handler wakener candidate_state line_ids =
  let%lwt candidates = Candidate_state.get_candidates candidate_state in
  let _ =
    match line_ids with
    | [||]          -> Lwt.wakeup_later wakener Confirmed_with_empty
    | _ as line_ids ->
        let ret = ref [] in
        Array.iter (fun index -> ret := (Vector.unsafe_get candidates index |> Candidate.text) :: !ret) line_ids;
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

let finalizers = ref []

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
      let candidate_state = Candidate_state.make () in
      let async_reader = Async_line_reader.make () in

      let monad =
        let%lwt window = create_window () in

        let box = new Widget_candidate_box.t () in
        let information_line = new Widget_information_line.t () in
        let read_line = new Widget_read_line.t ?query:option.query () in
        let module TR = Timestamp.Make (struct
          let now () = Unix.time () |> Int64.of_float
        end) in
        let module I = (val TR.make ()) in
        let hub = Event_hub.make (module I) in
        let available_filters = app_state.available_filters |> List.map ~f:fst in
        let term =
          new Widget_main.t
            ~box:(box :> LTerm_widget.t)
            ~read_line:(read_line :> LTerm_widget.t)
            ~information_line:(information_line :> LTerm_widget.t)
            ~available_filters ~event_hub:hub ()
        in
        information_line#set_filter_name @@ App_state.name_of_filter Widget_main.Partial_match;
        box#set_matcher app_state.matcher;

        Option.iter
          (fun path ->
            let observer, finalizer = Event_recorder.init path in
            add_finalizer finalizer;
            hub |> Event_hub.add_observer observer |> ignore)
          option.Cli_option.record_event_path;

        Option.iter
          (fun path ->
            let replay = Event_replayer.init path hub in
            Lwt.async (fun () -> replay))
          option.Cli_option.replay_event_path;

        React.S.changes candidate_state.signal
        |> Lwt_react.E.map_s (fun candidate ->
               let number = App_state.count_of_matches app_state in
               information_line#set_number_of_candidates number;

               match candidate with
               | [ candidate ] ->
                   App_state.push_line ~candidate app_state;
                   app_state.all_candidates |> box#set_candidates |> Lwt.return
               | _             -> Lwt.return_unit)
        |> Lwt_react.E.keep;

        (* define event and handler *)
        React.S.changes read_line#text
        |> Lwt_react.E.map_s (fun text -> selection_event_handler app_state box information_line text)
        |> Lwt_react.E.keep;
        React.S.changes term#switch_filter
        |> Lwt_react.E.map_s (change_filter_handler app_state information_line ~before_change)
        |> Lwt_react.E.keep;
        let waiter, wakener = Lwt.task () in
        React.S.changes term#quit |> React.E.map (quit_handler wakener) |> Lwt_react.E.keep;
        React.S.changes box#current_candidates
        |> Lwt_react.E.map_s (confirm_candidate_handler wakener candidate_state)
        |> Lwt_react.E.keep;

        (* running asynchronous data reading *)
        let read_async, close_read_async = Async_line_reader.read_async Lwt_unix.stdin async_reader in
        let write_async = Candidate_state.write_async (Async_line_reader.mailbox async_reader) candidate_state in
        add_finalizer close_read_async;
        Lwt.async (fun () -> read_async);
        Lwt.async (fun () -> write_async);

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
      | Quit                 -> exit 130
      | Confirmed_with_empty -> exit 1
      | Confirm v            ->
          let delimiter = if option.print_nul then Char.chr 0 |> String.make 1 else "\n" in
          String.concat delimiter v |> Printf.printf "%s\n";
          exit 0)
