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

let event_handler window term info () =
  let module I = Types.Info in
  let render info' = Renderer.render window info'.Types.Info.candidates info'.Types.Info.selection in
  let text = Zed_edit.text term#edit |> Zed_rope.to_string in
  let info = Zed_string.to_utf8 text |> Filter.filter info in
  let info = React.S.value term#selection |> Filter.update_selection info in
  render info

let () =
  ignore (new Widget_candidate_box.t ());
  let monad =
    let%lwt window = LTerm.create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout in
    let is_a_tty = LTerm.is_a_tty window in
    let%lwt info = Lwt.(process_when_tty window is_a_tty >>= make_info) in
    let%lwt () = LTerm.clear_screen window in
    let term = new Read_line.read_line ~term:window ~history:[] ~exit_code:0 info in
    Renderer.render window info.Types.Info.candidates None;%lwt
    LTerm.goto window { LTerm_geom.row = 0; col = 0 };%lwt
    let select =
      React.E.select
        [ React.E.stamp (React.S.changes term#selection) (); React.E.stamp (Zed_edit.changes term#edit) () ]
    in
    Lwt_react.E.keep select;
    let%lwt _ = Lwt_react.E.map_s (event_handler window term info) select |> Lwt.return in
    term#run
  in
  let v = Lwt_main.run monad in
  Printf.printf "%s" @@ Zed_string.to_utf8 v
