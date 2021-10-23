type matched = int * int [@@deriving show, eq]

type id = int [@@deriving show, eq]

type t = {
  line : Line.t;
  matched : matched list;
  filtered : bool;
}
[@@deriving show, eq]

let id { line; _ } = line.id

let make ?(matched = []) ?(filtered = false) line = { line; matched; filtered }

let filtered { filtered; _ } = filtered

let text { line = { Line.text; _ }; _ } = text

let matched { matched; _ } = matched

let is_matched { matched; filtered; _ } = if not filtered then true else match matched with [] -> false | _ -> true
