(**pp -syntax camlp5o -package camlp5,sexplib *)

val input_file : string ref
val raw_token : Lexing.lexbuf -> Sexplib.Parser.token * Ploc.t
val token : Lexing.lexbuf -> (string * string) * Ploc.t
val lexer : (string * string) Plexing.lexer
val g : Grammar.g
val sexp_eoi : Sexp0.t Grammar.Entry.e
val parse_sexp_eoi : char Stream.t -> Sexp0.t
val of_string : string -> Sexp0.t
val input_sexp : in_channel -> Sexp0.t
val load_sexp : string -> Sexp0.t
