open CamomileLibraryDefault.Camomile

type id = int

type t = private {
  id : id;
  text : UTF8.t;
}

val make : id -> UTF8.t -> t
