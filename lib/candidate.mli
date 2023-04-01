open Camomile

type id = int [@@deriving show, eq]

type t = private {
  id : id;
  text : UTF8.t;
}
[@@deriving show, eq]

val make : id:id -> text:UTF8.t -> t
(** [make ~id ~text] make a candidate *)

val id : t -> id
(** [id t] access to id of line *)

val text : t -> UTF8.t
(** [text t] access to text *)
