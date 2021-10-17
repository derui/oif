open Oif_lib
open Std
open CamomileLibraryDefault.Camomile
module DF = CamomileLibraryDefault
module ReIntf = DF.Camomile.UReStr
module Re = ReIntf.Make (UTF8)

module type Migemo_arg = sig
  val migemo : Migemocaml.Migemo.t
end

module Make (A : Migemo_arg) = struct
  let unique_name = "Migemo"

  let filter ~source ~text =
    let queries =
      Filter.split_query text
      |> List.filter ~f:(fun v -> String.length v > 0)
      |> List.map ~f:(fun query -> Migemocaml.Migemo.query ~query A.migemo)
    in

    let source = source () in
    let candidates = Seq.filter_map (Matcher.query queries) source in
    candidates
end
