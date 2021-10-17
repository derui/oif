open Oif_lib
open Std

type matched = int * int

type selection = int * LTerm_style.t

type direction =
  | Next
  | Prev

type candidates = Candidate.t list

(* Infomations to filtering candidates. *)
module Info = struct
  type t = {
    current_candidate : string option;
    lines : Line.t list;
  }

  let empty = { current_candidate = None; lines = [] }

  let to_candidates info = List.map info.lines ~f:(fun v -> Candidate.make v)

  let init lines = { empty with lines = List.mapi (fun i l -> Line.make ~id:i ~text:l) lines }
end
