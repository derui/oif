open CamomileLibrary
open Std

type style = int * LTerm_style.t

type row = int

type selection = row * LTerm_style.t

type direction =
  | Next
  | Prev

module Candidate : sig
  type t = private {
    text : UTF8.t;
    styles : style list;
  }

  val make : ?styles:style list -> UTF8.t -> t

  val text : t -> UTF8.t

  val styles : t -> style list

  val map_text : f:(int -> UChar.t -> style list -> 'a) -> t -> 'a list

  val iter_text : f:(int -> UChar.t -> style list -> unit) -> t -> unit
end = struct
  type t = {
    text : UTF8.t;
    styles : style list;
  }

  let make ?(styles = []) text = { text; styles }

  let text { text; _ } = text

  let styles { styles; _ } = styles

  let index_range_of_text { text; _ } =
    let len = UTF8.length text in
    if len = 0 then [] else List.range ~stop:`exclusive 0 len

  let map_text ~f t =
    let range = index_range_of_text t in
    let f index = f index (UTF8.get t.text index) t.styles in
    List.map range ~f

  let iter_text ~f t =
    let range = index_range_of_text t in
    let f index = f index (UTF8.get t.text index) t.styles in
    List.iter range ~f
end

type candidates = Candidate.t list

(* The candidates for filtering *)

(* Infomations to filtering candidates. *)
module Info = struct
  type t = {
    mutable current_candidate : string option;
    mutable lines : UTF8.t list;
  }

  let empty = { current_candidate = None; lines = [] }

  let to_candidates info = List.map info.lines ~f:Candidate.make

  let init lines = { empty with lines }
end
