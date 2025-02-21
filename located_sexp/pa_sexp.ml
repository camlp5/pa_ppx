(**pp -syntax camlp5r -package sexplib,,camlp5.extend *)
(* calc.ml,v *)

value input_file = ref "" ;

value raw_token lb =
  let open Sexplib in
  let buf = Buffer.create 23 in
  let main = Sexplib.Lexer.main ~{buf=buf} in
  let () = Wslexer.token lb in
  let spos = Lexing.lexeme_end lb in
  let tok = main lb in
  let epos = Lexing.lexeme_end lb in
  (tok, Ploc.make_loc input_file.val 1 0 (spos, epos) "")
;

value token lb =
  let (tok,loc) = raw_token lb in
  let tok = match tok with [
      Sexplib.Parser.STRING s -> ("STRING",s)
    | LPAREN -> ("","(")
    | RPAREN -> ("",")")
    | EOF -> ("EOI","EOI")
    | HASH_SEMI -> ("","#;")
      ]
  in
  (tok, loc)
;

value lexer = Plexing.lexer_func_of_ocamllex_located token ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None ; Plexing.kwds = Hashtbl.create 23 } ;

value g = Grammar.gcreate lexer;
value sexp_eoi = Grammar.Entry.create g "sexp_eoi";

EXTEND
  GLOBAL: sexp_eoi;

  sexp: [
    [ s = STRING -> Sexp0.Atom loc s
    | "(" ; ")" -> Sexp0.List loc []
    | "(" ; l = LIST0 sexp ; ")" -> Sexp0.List loc l
    | "#;" ; _ = sexp ; e = sexp -> e
    ]
  ]
  ;

  sexp_eoi: [ [ x = sexp; EOI -> x ] ];

END;

value parse_sexp_eoi = Grammar.Entry.parse sexp_eoi ;
value of_string s = s |> Stream.of_string |> parse_sexp_eoi ;

value input_sexp ic =
  ic |> Stream.of_channel |> Grammar.Entry.parse sexp_eoi
;

value load_sexp fname =
  let ic = open_in fname in
  let old_input_file = input_file.val in
  try do {
    input_file.val := fname ;
    let rv = input_sexp ic
    in do {
      close_in ic ;
      input_file.val := old_input_file ;
      rv
    }
  }
  with e -> do {
    close_in ic ;
    input_file.val := old_input_file ;
    raise e
  }
;
