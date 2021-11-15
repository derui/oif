open Oif_lib
open Std
open Oif

type t = {
  mutable current_filter : (module Filter.S);
  mutable available_filters : (Widget_main.filter * (module Filter.S)) list;
  mutable current_query : string option;
  all_candidates : Candidate_array.t;
  mutable current_candidates : Candidate_array.t;
  change_filter_mutex : Lwt_mutex.t;
}

let make ~current_filter ~available_filters =
  {
    current_filter;
    available_filters;
    current_query = None;
    current_candidates = Candidate_array.empty ();
    all_candidates = Candidate_array.empty ();
    change_filter_mutex = Lwt_mutex.create ();
  }

let update_available_filters t filters = t.available_filters <- filters

let current_candidates t = match t.current_query with None -> t.all_candidates | Some _ -> t.current_candidates

let push_line ~candidate t =
  let module F = (val t.current_filter) in
  t.all_candidates |> Candidate_array.push ~value:candidate;
  match t.current_query with
  | None       -> ()
  | Some query -> (
      let candidate = F.filter ~source:(fun () -> Seq.return candidate.line) ~text:query |> List.of_seq in
      match candidate with [ candidate ] -> t.current_candidates |> Candidate_array.push ~value:candidate | _ -> ())

let update_query query t =
  t.current_query <- (if String.length query > 0 then Some query else None);
  let module F = (val t.current_filter) in
  match t.current_query with
  | None       -> t.current_candidates <- t.all_candidates
  | Some query ->
      t.current_candidates <-
        F.filter
          ~source:(fun () -> Candidate_array.to_seq t.all_candidates |> Seq.map (fun v -> v.Candidate.line))
          ~text:query
        |> Seq.filter ~f:Candidate.is_matched |> Candidate_array.of_seq

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
