(**pp -syntax camlp5r -package sexplib,,camlp5.extend *)

open Pa_ppx_utils ;

value input_file = Plexing.input_file ;

module Lexer = Plexer.Make(struct end) ;
Lexer.simplest_raw_strings.val := True ;
value g = Grammar.gcreate (Lexer.gmake ());

value sexp_eoi = Grammar.Entry.create g "sexp_eoi";
(*
value sexp_comment = Grammar.Entry.create g "sexp_comment";
value sexp_then_comments = Grammar.Entry.create g "sexp_then_comments";
 *)
EXTEND
  GLOBAL: sexp_eoi (* sexp sexp_comment sexp_then_comments *);

  sexp: [
    [ s = STRING -> Sexp0.Atom loc (Std.unescape_string s)
    | s = LIDENT -> Sexp0.Atom loc s
    | s = UIDENT -> Sexp0.Atom loc s
    | rs = RAWSTRING ->
       let (_, s) = Asttools.split_rawstring rs in
       Sexp0.Atom loc (Std.unescape_string s)
    | "(" ; ")" -> Sexp0.List loc []
    | "(" ; l = LIST1 sexp ; ")" -> Sexp0.List loc l
    ]
  ]
  ;
  sexp_eoi: [ [ x = sexp ; EOI -> x ] ];

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
