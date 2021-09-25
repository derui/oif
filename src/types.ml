open Std
open Oif_lib

type matched = int * int

type selection = int * LTerm_style.t

type direction =
  | Next
  | Prev

type candidates = Candidate.t list

(* Infomations to filtering candidates. *)
module Info = struct
  type t = {
    mutable current_candidate : string option;
    mutable lines : Line.t list;
  }

  let empty = { current_candidate = None; lines = [] }

  let to_candidates info = List.map info.lines ~f:(fun v -> Candidate.make v)

  let init lines = { empty with lines = List.mapi (fun i l -> Line.make i l) lines }
end
