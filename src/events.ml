open Oif_lib
(** This module provides functionally for all actions in Oif. *)

(** type of events. *)
type kind = Key of LTerm_key.t

type t = {
  kind : kind;
  timestamp : Timestamp.t;
}

let make_key_event ~key ~timestamp = { kind = Key key; timestamp }
