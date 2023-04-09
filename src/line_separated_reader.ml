include struct
  type t = { line_mailbox : string Lwt_mvar.t }

  let make () = { line_mailbox = Lwt_mvar.create_empty () }

  let take_candidate t = Lwt_mvar.take t.line_mailbox

  let open_channel_for_async fd =
    let duplicated = Lwt_unix.dup fd in
    Lwt_unix.set_blocking duplicated false;
    Lwt_io.of_fd ~mode:Lwt_io.Input duplicated

  let read_candidates_async t ~fd =
    let channel = Lwt_unix.of_unix_file_descr fd |> open_channel_for_async in
    let mvar = t.line_mailbox in
    let open Lwt in
    let reader =
      catch
        (fun () ->
          let rec read_lines channel () =
            catch
              (fun () ->
                let open Lwt.Infix in
                Lwt_io.read_line_opt channel
                >>= (function None -> return_unit | Some line -> Lwt_mvar.put mvar line)
                >>= read_lines channel)
              (fun _ -> return_unit)
          in
          read_lines channel ())
        (fun _ -> return_unit)
    in
    let close () = Lwt_io.close channel |> ignore_result in
    (reader, close)
end
