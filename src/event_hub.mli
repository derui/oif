(** {!Event_hub} provides functions and systems to send and dispatch event *)

type 'event t

type 'event observer = 'event -> unit

type deleter = unit -> unit

val make : unit -> 'event t
(** [make ()] get new instance of event hub for [event] type *)

val dispatch : 'event -> 'event t -> unit
(** [dispatch event t] dispatch event to observers that is registered before *)

val add_observer : 'event observer -> 'event t -> deleter
(** [add_observer observer t] add [observer] to [t] instance. When stop observing via [observer], call deleter that is
    returned from this function *)
