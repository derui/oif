type t = { line_mailbox : string Lwt_mvar.t }

let make () = { line_mailbox = Lwt_mvar.create_empty () }

let mailbox t = t.line_mailbox

let open_channel_for_async fd =
  let duplicated = Lwt_unix.dup fd in
  Lwt_unix.set_blocking ~set_flags:true duplicated false;
  duplicated

let read_async fd t =
  let channel = open_channel_for_async fd in
  let mvar = t.line_mailbox in
  let reader =
    let rec read_lines channel =
      Lwt.catch
        (fun () ->
          let%lwt line = Lwt_io.read_line channel in
          Lwt_mvar.put mvar line;%lwt
          Lwt_unix.sleep 0.0;%lwt
          read_lines channel)
        (fun _ -> Lwt.return_unit)
    in
    read_lines (Lwt_io.of_fd ~mode:Lwt_io.Input channel)
  in
  let close () = Lwt_unix.close channel |> Lwt.ignore_result in
  (reader, close)
