open Oif_lib

open CamomileLibraryDefault.Camomile
(** This module provides functionally for all actions in Oif. *)

(** type of events. *)
type kind = Key of LTerm_key.t

type t = {
  kind : kind;
  timestamp : Timestamp.t;
}

type key_event_json = {
  code : string;
  control : bool;
  shift : bool;
  meta : bool;
  timestamp : string;
}
[@@deriving protocol ~driver:(module Protocol_conv_json.Json)]

let to_json_code = function
  | LTerm_key.Char ch -> UChar.code ch |> string_of_int
  | Enter             -> "Enter"
  | Escape            -> "Escape"
  | Tab               -> "Tab"
  | Up                -> "Up"
  | Down              -> "Down"
  | Left              -> "Left"
  | Right             -> "Right"
  | F1                -> "F1"
  | F2                -> "F2"
  | F3                -> "F3"
  | F4                -> "F4"
  | F5                -> "F5"
  | F6                -> "F6"
  | F7                -> "F7"
  | F8                -> "F8"
  | F9                -> "F9"
  | F10               -> "F10"
  | F11               -> "F11"
  | F12               -> "F12"
  | Next_page         -> "Next_page"
  | Prev_page         -> "Prev_page"
  | Home              -> "Home"
  | End               -> "End"
  | Insert            -> "Insert"
  | Delete            -> "Delete"
  | Backspace         -> "Backspace"

let of_json_code = function
  | "Enter"     -> LTerm_key.Enter
  | "Escape"    -> Escape
  | "Tab"       -> Tab
  | "Up"        -> Up
  | "Down"      -> Down
  | "Left"      -> Left
  | "Right"     -> Right
  | "F1"        -> F1
  | "F2"        -> F2
  | "F3"        -> F3
  | "F4"        -> F4
  | "F5"        -> F5
  | "F6"        -> F6
  | "F7"        -> F7
  | "F8"        -> F8
  | "F9"        -> F9
  | "F10"       -> F10
  | "F11"       -> F11
  | "F12"       -> F12
  | "Next_page" -> Next_page
  | "Prev_page" -> Prev_page
  | "Home"      -> Home
  | "End"       -> End
  | "Insert"    -> Insert
  | "Delete"    -> Delete
  | "Backspace" -> Backspace
  | _ as v      -> Char (UChar.of_int @@ int_of_string v)

let make_key_event ~key ~timestamp = { kind = Key key; timestamp }

let to_json { kind; timestamp } =
  match kind with
  | Key key ->
      let for_json =
        {
          code = key.code |> to_json_code;
          control = key.control;
          shift = key.shift;
          meta = key.meta;
          timestamp = Timestamp.to_int64 timestamp |> Int64.to_string;
        }
      in
      key_event_json_to_json for_json

let of_json json =
  let open Std.Result.Let_syntax in
  let converted =
    let* json = json |> key_event_json_of_json in
    let timestamp = json.timestamp |> Int64.of_string |> Timestamp.of_int64 in
    Ok
      {
        kind =
          Key { LTerm_key.code = of_json_code json.code; control = json.control; shift = json.shift; meta = json.meta };
        timestamp;
      }
  in

  Result.to_option converted
