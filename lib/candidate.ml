open Camomile

type id = int [@@deriving show, eq]

type t = {
  id : id;
  text : UTF8.t;
}

let id { id; _ } = id

let text { text; _ } = text

let make ~id ~text = { id; text }

let equal v1 v2 = v1.id = v2.id

let show t = Printf.sprintf "{id=%d,text=%s}" t.id t.text

let pp fmt t = Format.fprintf fmt "%s" @@ show t
