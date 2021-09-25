open CamomileLibraryDefault.Camomile

type matched = int * int

type id = int

type t = private {
  line : Line.t;
  matched : matched list;
}

val make : ?matched:matched list -> Line.t -> t
(** [make ?matched line] make a candidate *)

val id : t -> id
(** [id t] access to id of line *)

val text : t -> UTF8.t
(** [text t] access to text *)

val matched : t -> matched list
(** [matched t] access to matched *)
