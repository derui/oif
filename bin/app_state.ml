open Oif_lib
open Std
open Oif

type t = {
  mutable current_filter : (module Filter.S);
  mutable available_filters : (Widget_main.filter * (module Filter.S)) list;
  mutable current_query : string option;
  all_candidates : Candidate.t Vector.t ref;
  matcher : Matcher.t;
  change_filter_mutex : Lwt_mutex.t;
}

let make ~current_filter ~available_filters =
  let all_candidates = ref @@ Vector.empty () in
  {
    current_filter;
    available_filters;
    current_query = None;
    matcher = Matcher.make ~candidates:all_candidates;
    all_candidates;
    change_filter_mutex = Lwt_mutex.create ();
  }

let update_available_filters t filters = t.available_filters <- filters

let push_line ~candidate t =
  let module F = (val t.current_filter) in
  !(t.all_candidates) |> Vector.push ~value:candidate

let update_query query t =
  t.current_query <- (if String.length query > 0 then Some query else None);
  let module F = (val t.current_filter) in
  let query = Option.value ~default:"" t.current_query in
  Matcher.apply_filter ~filter:t.current_filter ~query t.matcher

let find_filter filter t = List.find ~f:(fun (filter', _) -> filter = filter') t.available_filters

let name_of_filter = function Widget_main.Partial_match -> "Partial match" | Widget_main.Migemo -> "Migemo"

let change_filter t filter =
  Lwt_mutex.with_lock t.change_filter_mutex (fun () ->
      t |> find_filter filter |> Option.iter (fun (_, filter) -> t.current_filter <- filter);
      Lwt.return_unit)

let current_filter_name t =
  Lwt_mutex.with_lock t.change_filter_mutex (fun () ->
      let module F = (val t.current_filter : Filter.S) in
      F.unique_name |> Lwt.return)
