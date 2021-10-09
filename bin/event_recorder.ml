open Oif_lib
open Std

let init path =
  let events = ref [] in
  let observer event = events := event :: !events in

  let finalizer () =
    let events = List.map ~f:Oif.Events.to_json !events |> List.rev in
    let events = `List events in
    Yojson.Safe.to_file path events
  in
  (observer, finalizer)
