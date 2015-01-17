open Core.Std

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
             let line = Types.Candidate.make line in
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
  begin
    let module G = LTerm_geom in
    let monad =
      window >>= fun window -> return (LTerm.is_a_tty window) >>= fun is_a_tty ->
      process_when_tty window is_a_tty >>= fun lines ->
      LTerm.clear_screen window >>= fun () ->
      let lines = List.rev lines in
      let term = new Read_line.read_line window [] 0 in
      Renderer.render window lines >>= fun () ->
      Lwt_react.E.map_s (Filter.filter window lines) term#text |> return >>= fun _ ->
      LTerm.goto window {LTerm_geom.row = 0;col = 0} >>= fun () ->
      term#run >>= fun s -> Printf.printf "foo %s"  s; return ()
    in 
    Lwt_main.run monad
  end
