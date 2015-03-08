open CamomileLibrary
open Core.Std

type style = int * LTerm_style.t

type row = int
type selection = row * LTerm_style.t
type direction = Next | Prev

module Candidate : sig
  type t = {
    text: UTF8.t;
    styles: style list;
  }

  val make : ?styles:style list -> UTF8.t -> t
  val text : t -> UTF8.t
  val styles : t -> style list
  val map_text : f:(int -> UChar.t -> style list -> 'a) -> t -> 'a list
end = struct
  type t = {
    text: UTF8.t;
    styles: style list;
  }

  let make ?(styles=[]) text = {text; styles}
  let text {text;_} = text
  let styles {styles;_} = styles

  let index_range_of_text {text;_} =
    let len = UTF8.length text in
    if len = 0 then [] else List.range ~stop:`exclusive 0 len
  let map_text ~f t =
    let range = index_range_of_text t in
    let f' index = f index (UTF8.get t.text index) t.styles in
    List.map range  ~f:f'
end

type candidates = Candidate.t option list
(* The candidates for filtering *)

(* Infomations to filtering candidates. *)
module Info = struct
  type t = {
    mutable selection: selection option;
    mutable text: UTF8.t;
    mutable candidates:candidates;
    mutable lines: UTF8.t list;
    mutable changed: bool;
  }

  let set_text t text = 
    let current = t.text in
    t.text <- text;
    t.changed <- text <> current;
    t

  let empty = {
    selection = None;
    text = "";
    candidates = [];
    lines = [];
    changed = false;
  }
end


