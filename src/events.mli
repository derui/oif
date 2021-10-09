open Oif_lib
(** This module provides functionally for all actions in Oif. *)

(** type of events. *)
type kind = Key of LTerm_key.t

type t = private {
  kind : kind;
  timestamp : Timestamp.t;
}

val make : kind:kind -> timestamp:Timestamp.t -> t
(** [make ~kind ~timestamp] make a event from [kind] at [timestamp]. *)

val kind_of_key : LTerm_key.t -> kind
(** [kind_of_key key] make kind for key *)

val to_json : t -> Yojson.Safe.t
(** [to_json t] convert event to JSON *)

val of_json : Yojson.Safe.t -> t option
(** [of_json json] convert event from JSON *)

val kind_of_lterm_event : LTerm_event.t -> kind option
(** [kind_of_lterm_event event ] convert [event] to [kind]. *)

val to_lterm_event : t -> LTerm_event.t
(** [to_lterm_event e] convert [e] to {!LTerm_event.t} *)
