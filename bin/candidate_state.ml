open Oif_lib

type t = {
  candidates : Candidate.t Vector.t;
  mutex : Lwt_mutex.t;
  signal : Candidate.t list React.signal;
  write_signal : Candidate.t list -> unit;
}

let make () =
  let signal, write_signal = React.S.create ~eq:(fun _ _ -> false) [] in
  { candidates = Vector.empty (); mutex = Lwt_mutex.create (); signal; write_signal }

let write_async mailbox t =
  let rec loop () =
    let%lwt line = Lwt_mvar.take mailbox in
    let%lwt candidate =
      Lwt_mutex.with_lock t.mutex (fun () ->
          let length = t.candidates |> Vector.length in
          let candidate = Candidate.make ~id:(succ length) ~text:line in
          Vector.push ~value:candidate t.candidates;
          Lwt.return candidate)
    in
    t.write_signal [ candidate ];
    loop ()
  in
  loop ()

let get_candidates t = Lwt_mutex.with_lock t.mutex (fun () -> t.candidates |> Lwt.return)
