open Oif_lib
open Std
open Camomile
module ReIntf = UReStr
module Re = ReIntf.Make (UTF8)

module type Migemo_arg = sig
  val migemo : Migemocaml.Migemo.t
end

module Make (A : Migemo_arg) = struct
  let unique_name = "Migemo"

  let filter ~candidate ~query =
    let queries =
      Filter.split_query query
      |> List.filter ~f:(fun v -> String.length v > 2)
      |> List.map ~f:(fun query -> Migemocaml.Migemo.query ~query A.migemo)
    in

    Filter.apply_matched queries candidate
end
