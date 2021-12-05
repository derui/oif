open Oif_lib

type candidate_id = Candidate.id

module Int_set = Set.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type t = { marked_indices : Int_set.t }

let equal v1 v2 = Int_set.equal v1.marked_indices v2.marked_indices

let empty = { marked_indices = Int_set.empty }

let is_empty { marked_indices } = Int_set.is_empty marked_indices

let toggle_mark ~index { marked_indices } =
  {
    marked_indices =
      (if Int_set.mem index marked_indices then Int_set.remove index marked_indices
      else Int_set.add index marked_indices);
  }

let marked_indices { marked_indices } = Int_set.to_seq marked_indices

let is_marked ~index { marked_indices } = Int_set.mem index marked_indices
