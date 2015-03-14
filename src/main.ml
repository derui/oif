open Core.Std
open CamomileLibrary

let reopen_in chan fname =
  let open Lwt in 
  Lwt_unix.openfile fname [Lwt_unix.O_RDONLY] 0o666 >>= fun fd2 ->
  Lwt_unix.dup2 fd2 chan;
  Lwt_unix.close fd2

let load_lines channel =
  let open Lwt in
  catch (fun () ->
    let rec read_lines channel lines =
      catch (fun () ->
        Lwt_io.read_line channel >>= fun line ->
        read_lines channel (line :: lines)
      ) (fun _ -> return lines)
    in
    read_lines channel []
  ) (fun _ -> return [])

let process_when_tty window is_a_tty =
  let open Lwt in
  if is_a_tty then return []
  else begin 
    load_lines Lwt_io.stdin >>= fun lines ->
    reopen_in Lwt_unix.stdin "/dev/tty" >>= fun () ->
    let in_chan = Lwt_io.of_fd ~mode:Lwt_io.input Lwt_unix.stdin in
    LTerm.set_io ~incoming_channel:in_chan window >>= fun () -> return lines
  end

let make_info lines = 
  let open Lwt in
  let lines = List.rev lines in
  return {Types.Info.empty with
    Types.Info.candidates = List.map lines ~f:(Types.Candidate.make) |> List.map ~f:Option.some;
    lines;
  }

let event_handler window term info () = 
  let module I = Types.Info in
  let render info' =
    Renderer.render window info'.Types.Info.candidates info'.Types.Info.selection in
  let text = Zed_edit.text term#edit |> Zed_rope.to_string  in
  let info = Filter.filter info text in
  let info = React.S.value term#selection |> Filter.update_selection info in
  render info

let () =
  let open Lwt in
  let window = LTerm.create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout in

  begin
    let monad = window >>= fun window -> return (LTerm.is_a_tty window) >>= fun is_a_tty ->
      process_when_tty window is_a_tty >>= make_info >>= fun info ->
      LTerm.clear_screen window >>= fun () ->
      let term = new Read_line.read_line window [] 0 info in
      let module S = LTerm_style in
      Renderer.render window info.Types.Info.candidates None >>= fun () ->
      LTerm.goto window {LTerm_geom.row = 0;col = 0} >>= fun () ->

      let select = React.E.select 
        [React.E.stamp (React.S.changes term#selection) () ;
         React.E.stamp (Zed_edit.changes term#edit) ()
        ] in
      Lwt_react.E.keep select;
      Lwt_react.E.map_s (event_handler window term info) select |> return >>= fun _ -> term#run
    in 
    let v = Lwt_main.run monad in
    Printf.printf "%s" v
  end
