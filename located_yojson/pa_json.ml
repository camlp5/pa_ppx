(**pp -syntax camlp5r -package camlp5.extend *)
(* calc.ml,v *)
value g = Grammar.gcreate (Pcaml.Lexer.gmake ());

value json_eoi = Grammar.Entry.create g "json_eoi";

value make_int ~neg x =
  try
    let n = int_of_string x in
    if neg then `Int (-n)
    else `Int n
  with [
      Failure _ ->
      if neg then `Intlit ("-"^x)
      else `Intlit x
    ]
;

value make_float ~neg x =
  try
    let n = float_of_string x in
    if neg then `Float (-. n)
    else `Float n
  with [
      Failure _ ->
      if neg then `Intlit ("-"^x)
      else `Intlit x
    ]
;

EXTEND
  GLOBAL: json_eoi;

  json: [
    [ s = STRING -> (loc, `String s)
    | "null" -> (loc, `Null)
    | s = INT -> (loc, make_int ~{neg=False} s)
    | f = FLOAT -> (loc, make_float ~{neg=False} f)
    | "-" ; s = INT -> (loc, make_int ~{neg=True} s)
    | "-" ; f = FLOAT -> (loc, make_float ~{neg=True} f)
    | b = ["true" -> True | "false" -> False] -> (loc, `Bool b)
    | "[" ; l = LIST0 json SEP "," ; "]" -> (loc, `List l)
    | "{" ; l = LIST0 [ s = STRING ; ":" ; j = json -> (s,j) ] SEP "," ; "}" -> (loc, `Assoc l)
    ]
  ]
  ;
  json_eoi: [ [ x = json ; EOI -> x ] ];

END;

value parse_json_eoi = Grammar.Entry.parse json_eoi ;
value of_string s = s |> Stream.of_string |> parse_json_eoi ;

value input_json ic =
  ic |> Stream.of_channel |> Grammar.Entry.parse json_eoi
;

value load_json fname =
  let ic = open_in fname in
  let old_input_file = Plexing.input_file.val in
  try do {
    Plexing.input_file.val := fname ;
    let rv = input_json ic
    in do {
      close_in ic ;
      Plexing.input_file.val := old_input_file ;
      rv
    }
  }
  with e -> do {
    close_in ic ;
    Plexing.input_file.val := old_input_file ;
    raise e
  }
;
