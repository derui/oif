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

let process_when_tty window is_a_tty =
  if is_a_tty then Lwt.return []
  else
    let%lwt lines = load_lines Lwt_io.stdin in
    let%lwt tty_fd = open_tty "/dev/tty" in
    let in_chan = Lwt_io.of_fd ~mode:Lwt_io.input tty_fd in
    let%lwt () = LTerm.set_io ~incoming_fd:tty_fd ~incoming_channel:in_chan window in
    Lwt.return lines

let make_info lines = Lwt.return @@ Types.Info.init lines

let selection_event_handler box info text _ =
  let info = Filter.filter info text in
  let module I = Types.Info in
  box#set_candidates info.I.candidates

let () =
  ignore (new Widget_candidate_box.t ());
  let monad =
    let%lwt window = LTerm.create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout in
    let is_a_tty = LTerm.is_a_tty window in
    let%lwt info = Lwt.(process_when_tty window is_a_tty >>= make_info) in
    let box = new Widget_candidate_box.t () in
    let read_line = new Widget_read_line.t () in
    let term = new Main_widget.t ~box:(box :> LTerm_widget.t) ~read_line:(read_line :> LTerm_widget.t) () in
    let%lwt window_size = LTerm.get_size window in
    box#set_candidates info.Types.Info.candidates;
    LTerm.render window (LTerm_draw.make_matrix window_size);%lwt
    LTerm.goto window { LTerm_geom.row = 0; col = 0 };%lwt

    (* define event and handler *)
    let change_text = React.S.changes read_line#text in
    Lwt_react.E.keep change_text;
    let change_candidate = React.S.changes box#current_candidate in
    Lwt_react.E.keep change_candidate;
    let%lwt _ =
      Lwt_react.E.l2
        (fun text candidate -> selection_event_handler box info text candidate)
        change_text change_candidate
      |> Lwt.return
    in

    let waiter, _ = Lwt.task () in
    LTerm_widget.run window term waiter
  in
  let _ = Lwt_main.run monad in
  ()

(* Printf.printf "%s" @@ Zed_string.to_utf8 v *)
