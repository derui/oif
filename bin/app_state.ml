open Oif_lib

type t = {
  mutable current_filter : (module Filter.S);
  available_filters : (module Filter.S) list;
  mutable current_query : string option;
}

let make ~current_filter ~available_filters = { current_filter; available_filters; current_query = None }
