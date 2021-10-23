open CamomileLibraryDefault.Camomile

type matched = int * int [@@deriving show, eq]

type id = int [@@deriving show, eq]

type t = private {
  line : Line.t;
  matched : matched list;
  filtered : bool;
}
[@@deriving show, eq]

val make : ?matched:matched list -> ?filtered:bool -> Line.t -> t
(** [make ?matched ?filtered line] make a candidate *)

val id : t -> id
(** [id t] access to id of line *)

val filtered : t -> bool
(** [filtered t] access to [filtered] *)

val text : t -> UTF8.t
(** [text t] access to text *)

val matched : t -> matched list
(** [matched t] access to matched *)

val is_matched : t -> bool
(** [is_matched t] returns [t] is matched from query *)
