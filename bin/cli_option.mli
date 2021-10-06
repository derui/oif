type t = {
  prompt : string option;
  migemo_dict_directory : string option;
  query : string option;
  record_event_path : string option;
}

val empty : t
(** empty option *)

val parse : (t -> unit) -> unit
(** parse options and get parsed it. *)
