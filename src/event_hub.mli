open Oif_lib
(** {!Event_hub} provides functions and systems to send and dispatch event *)

type t

type observer = Events.t -> unit

type deleter

val make : (module Timestamp.Instance) -> t
(** [make ()] get new instance of event hub for [event] type *)

val dispatch : Events.kind -> t -> unit
(** [dispatch kind t] dispatch event to observers that is registered before *)

val add_observer : observer -> t -> deleter
(** [add_observer observer t] add [observer] to [t] instance. When stop observing via [observer], call deleter that is
    returned from this function *)

val call_deleter: deleter -> unit
                               (** [call_deleter deleter] calls given deleter *)
