open Oif_lib
open Std
open Oif

type t = {
  mutable current_filter : (module Filter.S);
  mutable available_filters : (Widget_main.filter * (module Filter.S)) list;
  mutable current_query : string option;
  matcher : New_matcher.t;
  change_filter_mutex : Lwt_mutex.t;
  candidate_mutex : Lwt_mutex.t;
  count_of_matches : int React.signal;
  set_count_of_matches : int -> unit;
}

let make ~current_filter ~available_filters =
  let count_of_matches, set_count_of_matches = React.S.create 0 in
  {
    current_filter;
    available_filters;
    current_query = None;
    matcher = New_matcher.make ();
    change_filter_mutex = Lwt_mutex.create ();
    candidate_mutex = Lwt_mutex.create ();
    count_of_matches;
    set_count_of_matches;
  }

let update_available_filters t filters = t.available_filters <- filters

let update_query query t =
  t.current_query <- (if String.length query > 0 then Some query else None);
  let module F = (val t.current_filter) in
  let query = Option.value ~default:"" t.current_query in
  Lwt_mutex.with_lock t.candidate_mutex (fun () ->
      New_matcher.apply_filter ~filter:t.current_filter ~query t.matcher;%lwt
      t.set_count_of_matches (New_matcher.matched_results t.matcher |> Array.length) |> Lwt.return)

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

let count_of_matches { matcher; _ } = New_matcher.matched_results matcher |> Array.length

let matched_results { matcher; _ } = New_matcher.matched_results matcher

let write_async mailbox t =
  let rec loop () =
    let%lwt line = Lwt_mvar.take mailbox in
    let%lwt () =
      Lwt_mutex.with_lock t.candidate_mutex (fun () ->
          let length = t.matcher |> New_matcher.all_match_results |> Array.length in
          let candidate = Candidate.make ~id:(succ length) ~text:line in
          t.matcher |> New_matcher.add_candidate ~candidate ~filter:t.current_filter;%lwt
          New_matcher.matched_results t.matcher |> Array.length |> t.set_count_of_matches |> Lwt.return)
    in
    loop ()
  in
  loop ()
