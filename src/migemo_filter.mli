open Oif_lib.Filter

(** Arguments for migemo filter. *)
module type Migemo_arg = sig
  val migemo : Migemocaml.Migemo.t
end

(** {!Migemo} provides to filter matched user input partially. *)
module Make : functor (_ : Migemo_arg) -> S
