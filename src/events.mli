open Oif_lib
(** This module provides functionally for all actions in Oif. *)

(** type of events. *)
type kind = Key of LTerm_key.t

type t = private {
  kind : kind;
  timestamp : Timestamp.t;
}

val make_key_event : key:LTerm_key.t -> timestamp:Timestamp.t -> t
(** [make_key_event ~key ~timestamp] get new instance of key event *)

val to_json : t -> Yojson.Safe.t
(** [to_json t] convert event to JSON *)

val of_json : Yojson.Safe.t -> t option
(** [of_json json] convert event from JSON *)
