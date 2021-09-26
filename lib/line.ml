open CamomileLibraryDefault.Camomile

type id = int

type t = {
  id : id;
  text : UTF8.t;
}

let make ~id ~text = { id; text }
