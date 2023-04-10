module V = Vector

type candidates = Candidate.t Vector.t

type match_results = Match_result.t Vector.t

type matched_index = int

type t = {
  candidates : candidates;
  mutable match_results : match_results;
  mutable matched_indices : matched_index Vector.t;
  mutable last_query : string option;
}

let make () = { candidates = V.empty (); match_results = V.empty (); matched_indices = V.empty (); last_query = None }

let apply_filter_to_candidate' ~filter ~query ~candidate =
  let module F = (val filter : Filter_intf.S) in
  F.filter ~query ~candidate |> Lwt.return

let add_candidate ~candidate ~filter t =
  let%lwt result =
    match t.last_query with
    | None -> Lwt.return @@ Match_result.no_query ()
    | Some query -> apply_filter_to_candidate' ~filter ~candidate ~query
  in
  if Match_result.is_matched result then V.push t.matched_indices ~value:(V.length t.candidates) else ();
  V.push t.match_results ~value:result;
  V.push t.candidates ~value:candidate;
  Lwt.return_unit

let apply_filter ~filter ~query t =
  let module F = (val filter : Filter_intf.S) in
  let matched_indices = V.empty () in
  let%lwt results =
    t.candidates |> V.to_seq |> List.of_seq
    |> Lwt_list.mapi_p (fun index v ->
           let%lwt result = apply_filter_to_candidate' ~filter ~candidate:v ~query in
           Lwt.return (index, result))
  in
  let match_results = V.make (V.length t.candidates) Match_result.empty in
  results
  |> List.iter (fun (index, result) ->
         V.unsafe_set match_results index result;
         if Match_result.is_matched result then V.push ~value:index matched_indices else ());
  t.match_results <- match_results;
  t.matched_indices <- matched_indices;
  t.last_query <- Some query;
  Lwt.return_unit

let all_match_results { match_results; candidates; _ } =
  assert (V.length match_results = V.length candidates);

  match_results |> V.to_array |> Array.mapi (fun index result -> (V.unsafe_get candidates index, result))

let matched_results t =
  t.matched_indices |> V.to_array
  |> Array.map (fun index -> (V.unsafe_get t.candidates index, V.unsafe_get t.match_results index))

let matched_count t = t.matched_indices |> V.length
