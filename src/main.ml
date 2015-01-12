open CamomileLibrary
open Core.Std

class read_line ~term ~history ~exit_code =
  let text_s, set_text = React.E.create () in
object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term as super

  method show_box = false

  method text = text_s
  method set_text = set_text

  method private exec actions =
    let open Lwt in
    match actions with
    | [] -> super#exec actions
    | action :: rest -> begin match action with
      | LTerm_read_line.Accept -> return (Zed_edit.text self#edit |> Zed_rope.to_string)
      | _ -> begin self#send_action action; self#exec rest end
    end

  initializer
  let signal = LTerm_text.of_string "QUERY> " |> React.S.const in
  self#set_prompt signal;
  React.E.map (fun _ ->
    let text = Zed_edit.text self#edit |> Zed_rope.to_string  in
    self#set_text text
  ) (Zed_edit.changes self#edit) |> ignore;
  Zed_macro.add self#macro LTerm_read_line.Accept;

end

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
        Lwt_io.read_line channel >>= fun line -> read_lines channel (line :: lines)
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

let render window lines =
  let open Lwt in
  LTerm.goto window {LTerm_geom.row = 1; col = 0} >>= fun () ->
  let size = LTerm.size window in
  let mat = LTerm_draw.make_matrix size in
  let ctx = LTerm_draw.context mat size in
  List.fold lines ~init:1 ~f:(fun current line ->
    LTerm_draw.draw_string ctx current 0 line;
    succ current
  ) |> ignore;
  LTerm.render window mat >>= fun () ->
  LTerm.goto window {LTerm_geom.row = 0; col = 0}

let filter window lines text =
  render window (text :: lines)
 
let () =
  let open Lwt in
  let window = LTerm.create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout in
  begin
    let module G = LTerm_geom in
    let monad =
      window >>= fun window -> return (LTerm.is_a_tty window) >>= fun is_a_tty ->
      process_when_tty window is_a_tty >>= fun lines ->
      LTerm.clear_screen window >>= fun () ->
      let lines = List.rev lines in
      let term = new read_line window [] 0 in
      render window lines >>= fun () ->
      Lwt_react.E.map_s (filter window lines) term#text |> return >>= fun _ ->
      LTerm.goto window {LTerm_geom.row = 0;col = 0} >>= fun () ->
      term#run >>= fun s -> Printf.printf "foo %s"  s; return ()
    in 
    Lwt_main.run monad
  end
