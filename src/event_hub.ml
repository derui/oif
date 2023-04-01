open Oif_lib

type observer = Events.t -> unit

type t = {
  observers : observer list ref;
  timestamper : (module Timestamp.Instance);
}

type deleter = unit -> unit

(** [make ()] get new instance of event hub for [event] type *)
let make (module T : Timestamp.Instance) = { observers = ref []; timestamper = (module T) }

let dispatch event { observers; timestamper = (module T) } =
  let timestamp = T.(timestamp instance) in
  let event = Events.make ~kind:event ~timestamp in
  List.iter (fun v -> v event) !observers

let add_observer observer { observers; _ } =
  let deleter () =
    let list = !observers in
    observers := List.filter (fun v -> v != observer) list
  in
  observers := observer :: !observers;
  deleter

let call_deleter f = f ()
