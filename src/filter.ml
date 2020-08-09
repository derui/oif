open Std
include Filter_intf

module Partial_match = struct
  let unique_name = "Partial match"

  let filter ~info ~text =
    let module I = Types.Info in
    let text = if String.length text > 0 then Printf.sprintf "^.*\\(%s\\)" text |> Option.some else None in
    let candidates =
      List.map info.I.lines ~f:(Text_match.decorate_to_match text)
      |> List.filter ~f:Option.is_some |> List.map ~f:Option.get
    in
    candidates
end

module Migemo (A : Migemo_arg) = struct
  let unique_name = "Migemo"

  let filter ~info ~text =
    let query = Migemocaml.Migemo.query ~query:text A.migemo |> Option.some in

    let module I = Types.Info in
    let candidates =
      List.map info.I.lines ~f:(Text_match.decorate_to_match query)
      |> List.filter ~f:Option.is_some |> List.map ~f:Option.get
    in
    candidates
end
