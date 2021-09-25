type matched = int * int

type id = int

type t = {
  line : Line.t;
  matched : matched list;
}

let id { line; _ } = line.id

let make ?(matched = []) line = { line; matched }

let text { line = { Line.text; _ }; _ } = text

let matched { matched; _ } = matched
