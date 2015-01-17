open CamomileLibrary

let filter window lines text =
  let text = Types.Candidate.make text in
  Renderer.render window (text :: lines)
 
