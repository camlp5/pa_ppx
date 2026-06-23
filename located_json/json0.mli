(**pp -syntax camlp5o *)

type _t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of t list
  | `Null
  | `String of string ]
and t = Ploc.t * _t

val loc_of_json : t -> Ploc.t
