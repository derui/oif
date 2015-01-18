open CamomileLibrary
open Core.Std

let filter window lines text =
  let text = Types.Candidate.make text in

  List.map lines ~f:(Text_match.decorate_to_match text) |> Renderer.render window 
