(**pp -syntax camlp5o -package pa_ppx_deriving_plugins.located_sexp *)

type foo = int -> int
  [@@deriving located_sexp_of]
