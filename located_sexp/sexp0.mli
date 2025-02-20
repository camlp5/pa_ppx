(**pp -syntax camlp5o -package sexplib *)

type t = Atom of Ploc.t * string | List of Ploc.t * t list
val loc_of_sexp : t -> Ploc.t
