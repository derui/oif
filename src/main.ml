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

let () =
  let open Lwt in
  let window = LTerm.create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout in
  LTerm_read_line.bind [{LTerm_key.code = LTerm_key.Char(UChar.of_char 'n');
                         control = true; meta = false; shift = false}]
                       [LTerm_read_line.Complete_bar_next];
  LTerm_read_line.bind [{LTerm_key.code = LTerm_key.Char(UChar.of_char 'p');
                         control = true; meta = false; shift = false}]
                       [LTerm_read_line.Complete_bar_prev];
  let info = ref Types.Info.empty in

  begin
    let monad =
      window >>= fun window -> return (LTerm.is_a_tty window) >>= fun is_a_tty ->
      process_when_tty window is_a_tty >>= fun lines ->
      let lines = List.rev lines in
      info := {!info with
                Types.Info.candidates = List.map lines ~f:(Types.Candidate.make) |> List.map ~f:Option.some;
                lines;
              };
      return () >>= fun () ->
      LTerm.clear_screen window >>= fun () ->
      let term = new Read_line.read_line window [] 0 in
      let module S = LTerm_style in
      Renderer.render window !info.Types.Info.candidates (0, Const.selection_style) >>= fun () ->
      LTerm.goto window {LTerm_geom.row = 0;col = 0} >>= fun () ->
      let selection = React.E.map (fun selection ->
                                   Filter.update_selection term !info selection) term#selection in
      let text = React.E.map (fun text -> Filter.filter term !info text) term#text in
      let render info' =
        info := info';
        Renderer.render window info'.Types.Info.candidates info'.Types.Info.selection in
      React.E.select [selection;text] |> React.E.l1 render |> return >>= fun _ ->
      term#run
    in 
    let v = Lwt_main.run monad in
    Printf.printf "%s" v
  end
