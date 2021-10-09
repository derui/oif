open Oif_lib
open Std
open Oif

let init path hub =
  let json = Yojson.Safe.from_file path in
  let events = Yojson.Safe.Util.(json |> to_list) |> List.filter_map ~f:Events.of_json in
  let rec loop = function
    | []          -> Lwt.return ()
    | e :: events ->
        let%lwt () = Event_hub.dispatch e.Events.kind hub |> Lwt.return in
        let%lwt () = Lwt_unix.sleep 0.1 in
        loop events
  in
  loop events
