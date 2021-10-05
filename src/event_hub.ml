type 'event observer = 'event -> unit

type 'event t = { observers : 'event observer list ref }

type deleter = unit -> unit

(** [make ()] get new instance of event hub for [event] type *)
let make () = { observers = ref [] }

let dispatch event { observers } = List.iter (fun v -> v event) !observers

let add_observer observer { observers } =
  let deleter () =
    let list = !observers in
    observers := List.filter (fun v -> v != observer) list
  in
  observers := observer :: !observers;
  deleter
