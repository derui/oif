open Oif_lib

type matched = int * int

type selection = int * LTerm_style.t

type direction =
  | Next
  | Prev

type candidates = Candidate.t list
