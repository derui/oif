include module type of Filter_intf

module Partial_match : S
(** {!Partial_match} provides to filter matched user input partially. *)

(** {!Migemo} provides to filter matched user input partially. *)
module Migemo (Migemo_arg : Migemo_arg) : S
