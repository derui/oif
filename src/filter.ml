open Std

let filter info text =
  let module I = Types.Info in
  let candidates = List.map info.I.lines ~f:(Text_match.decorate_to_match text) in
  candidates
