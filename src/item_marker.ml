open Oif_lib

module Int_set = Set.Make (struct
  type t = int

  let compare = Stdlib.compare
end)

type t = { marked_lines : Int_set.t }

let equal v1 v2 = Int_set.equal v1.marked_lines v2.marked_lines

let empty = { marked_lines = Int_set.empty }

let is_empty { marked_lines } = Int_set.is_empty marked_lines

let toggle_mark candidate { marked_lines } =
  let line_id = Candidate.id candidate in
  {
    marked_lines =
      (if Int_set.mem line_id marked_lines then Int_set.remove line_id marked_lines
      else Int_set.add line_id marked_lines);
  }

let marked_lines { marked_lines } = Int_set.to_seq marked_lines

let is_marked candidate { marked_lines } = Int_set.mem candidate.Candidate.id marked_lines
