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
  let map_text ~f t =
    let range = UTF8.length t.text |> List.range ~stop:`exclusive 0 in
    let f' index = f index (UTF8.get t.text index) t.styles in
    List.map range  ~f:f'
end


type candidates = Candidate.t list
(* The candidates for filtering *)

