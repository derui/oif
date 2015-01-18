open CamomileLibrary
open Core.Std

type style = int * LTerm_style.t

module Candidate = struct
  type t = {
    text: UTF8.t;
    styles: style list;
  }

  let make ?(styles=[]) text = {text; styles}
  let text {text;_} = text
  let styles {styles;_} = styles

  let index_range_of_text {text;_} = UTF8.length text |> List.range ~stop:`exclusive 0 
  let map_text ~f t =
    let range = index_range_of_text t in
    let f' index = f index (UTF8.get t.text index) t.styles in
    List.map range  ~f:f'

  let text_to_uchars t =
    let range = index_range_of_text t in
    let f memo i = (UTF8.look t.text i) :: memo in
    List.fold range ~init:[] ~f |> List.rev
end

type candidates = Candidate.t list
(* The candidates for filtering *)

