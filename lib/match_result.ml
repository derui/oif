type matched = int * int [@@deriving show, eq]

type t =
  | Empty
  | Matched of { matched : matched list }
[@@deriving show, eq]

let empty = Empty

let make ~matched = if List.length matched = 0 then Empty else Matched { matched }

let no_query () = Matched { matched = [] }

let matched = function Empty -> [] | Matched { matched } -> matched

let is_matched = function Empty -> false | Matched _ -> true
