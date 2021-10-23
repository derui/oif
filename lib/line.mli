open CamomileLibraryDefault.Camomile

type id = int [@@deriving show, eq]

type t = private {
  id : id;
  text : UTF8.t;
}
[@@deriving show, eq]

val make : id:id -> text:UTF8.t -> t
