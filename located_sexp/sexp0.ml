(**pp -syntax camlp5r -package sexplib *)

type t = [
    Atom of Ploc.t and string
  | List of Ploc.t and list t
  ]
;

value loc_of_sexp = fun [
  Atom loc _ -> loc
| List loc _ -> loc
]
;
