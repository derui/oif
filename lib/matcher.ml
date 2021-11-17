module V = Vector

type candidates = Candidate.t Vector.t ref

type match_results = Match_result.t Vector.t

type t = {
  candidates : candidates;
  mutable previous_candidate_size : int;
  mutable match_results : match_results;
  mutable matched_indices : int Vector.t;
}

let make ~candidates =
  { candidates; previous_candidate_size = 0; match_results = V.empty (); matched_indices = V.empty () }

let apply_filter ~filter ~query t =
  let module F = (val filter : Filter_intf.S) in
  let v = t.candidates in
  let matched_indices = V.empty () in
  let candidates_size = V.length !v in
  let match_results =
    if t.previous_candidate_size <> candidates_size then V.make (V.length !v) Match_result.empty else t.match_results
  in
  !v
  |> V.iteri ~f:(fun index v ->
         let result = F.filter ~query ~candidate:v in
         if Match_result.is_matched result then V.push ~value:index matched_indices else ();
         V.unsafe_set match_results index result);
  t.previous_candidate_size <- candidates_size;
  t.match_results <- match_results;
  t.matched_indices <- matched_indices

let match_results { match_results; _ } = match_results

let matched_indices t =
  let current_candidate_size = V.length !(t.candidates) in
  if t.previous_candidate_size = 0 then t.matched_indices <- V.init current_candidate_size (fun v -> v) else ();
  t.matched_indices
