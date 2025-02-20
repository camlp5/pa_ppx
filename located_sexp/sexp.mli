(**pp -syntax camlp5o -package sexplib,camlp5,pa_ppx.import *)

[%%import: Sexp0.t]

val loc_of_sexp : t -> Ploc.t
val to_sexplib_sexp : t -> Sexplib.Type.t
val of_sexplib_sexp : Ploc.t -> Sexplib.Type.t -> t
val to_string : t -> string
val of_string : string -> t
val equal : t -> t -> bool
