open Oif_lib

type t = {
  mutable current_filter : (module Filter.S);
  available_filters : (module Filter.S) list;
}
