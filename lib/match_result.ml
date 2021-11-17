type matched = int * int [@@deriving show, eq]

type t = {
  is_matched : bool;
  matched : matched list;
}
[@@deriving show, eq]

let empty = { is_matched = false; matched = [] }

let make ~matched = { matched; is_matched = (match matched with [] -> false | _ -> true) }

let no_query () = { matched = []; is_matched = true }

let matched { matched; _ } = matched

let is_matched { is_matched; _ } = is_matched
