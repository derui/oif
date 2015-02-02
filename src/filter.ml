open CamomileLibrary
open Core.Std

type 'a filter = Read_line.read_line -> Types.Info.t -> 'a -> Types.Info.t

let filter term info text =
  let module I = Types.Info in
  let candidates = List.map info.I.lines ~f:(Text_match.decorate_to_match text) in
  let selection = (term#current_selection () + 1, Const.selection_style) in
  {info with Types.Info.candidates; selection; text}

let update_selection term info selection =
  let selection = (selection + 1, Const.selection_style) in
  {info with Types.Info.selection }
