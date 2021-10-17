open Oif_lib

type t = {
  mutable lines : Line.t list;
  mutex : Lwt_mutex.t;
  signal : Line.t list React.signal;
  write_signal : Line.t list -> unit;
}

let make () =
  let signal, write_signal = React.S.create ~eq:(fun _ _ -> false) [] in
  { lines = []; mutex = Lwt_mutex.create (); signal; write_signal }

let write_async mailbox t =
  let rec loop () =
    let%lwt line = Lwt_mvar.take mailbox in
    Lwt_mutex.with_lock t.mutex (fun () ->
        let length = t.lines |> List.length in
        t.lines <- Line.make ~id:(succ length) ~text:line :: t.lines;
        t.lines |> List.rev |> t.write_signal;
        Lwt.return_unit);%lwt
    loop ()
  in
  loop ()

let get_lines t = Lwt_mutex.with_lock t.mutex (fun () -> t.lines |> List.rev |> Lwt.return)
