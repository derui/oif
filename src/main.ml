open Std

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

let selection_event_handler box info text =
  let candidates = Filter.filter info text |> List.filter ~f:Option.is_some |> List.map ~f:Option.get in
  box#set_candidates candidates

let confirm_candidate_handler wakener candidate = Lwt.wakeup wakener @@ Option.map Types.Candidate.text candidate

let create_window () =
  let%lwt tty_fd = open_tty "/dev/tty" in
  let in_chan = Lwt_io.of_fd ~mode:Lwt_io.input tty_fd in
  let out_chan = Lwt_io.of_fd ~mode:Lwt_io.output tty_fd in
  LTerm.create tty_fd in_chan tty_fd out_chan

let () =
  let option = ref Cli_option.empty in
  let () = Cli_option.parse (fun v -> option := v) in
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

    (* define event and handler *)
    let () =
      React.S.changes read_line#text
      |> React.E.map (fun text -> selection_event_handler box info text)
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
      exit 0
