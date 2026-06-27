(**pp -syntax camlp5r -package camlp5.extend *)
(* calc.ml,v *)
value g = Grammar.gcreate (Pcaml.Lexer.gmake ());

value json = Grammar.Entry.create g "json";
value json_eoi = Grammar.Entry.create g "json_eoi";
value json_or_eoi = Grammar.Entry.create g "json_or_eoi";
value json_list = Grammar.Entry.create g "json_list";
value json_list_eoi = Grammar.Entry.create g "json_list_eoi";

value make_int ~{neg} x =
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

value make_float ~{neg} x =
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
  GLOBAL: json json_eoi json_or_eoi json_list json_list_eoi;

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
  json_or_eoi: [ [ x = json -> Some x | EOI -> None ] ];
  json_list: [ [ l = LIST0 json -> l ] ] ;
  json_list_eoi: [ [ x = json_list ; EOI -> x ] ];

END;
(*
value with_input_file pafun ~{file} =
  let ic = open_in file in
  let old_input_file = Plexing.input_file.val in
  try do {
    Plexing.input_file.val := file ;
    let strm = Stream.of_channel ic in
    let rv = pafun strm
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
 *)
value with_input_file g pafun consumer ~{file} =
  let ic = open_in file in
  let old_input_file = Plexing.input_file.val in
  try do {
    Plexing.input_file.val := file ;
    let cs = Stream.of_channel ic in
    let cspa = Grammar.parsable g cs in
    let strm = Stream.from (fun _ -> pafun cspa) in
    let rv = consumer strm
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

module type PAHELPER = sig
  type t = 'a ;
  value entry : Grammar.Entry.e t ;
  value parse : (Stream.t char) -> t ;
  value parse_parsable : Grammar.parsable -> t ;
  value of_string : string -> t ;
  value input : in_channel -> t ;
  value load : ~file:string -> t ;
end ;

module PAHelper(M : sig type t = 'a ; value entry : Grammar.Entry.e t ; end)
 : (PAHELPER with type t = M.t) = struct
  type t = M.t ;
  value entry = M.entry ;
  value parse = Grammar.Entry.parse entry ;
  value parse_parsable = Grammar.Entry.parse_parsable entry ;
  value of_string s = s |> Stream.of_string |> parse ;
  value input ic =
    ic |> Stream.of_channel |> Grammar.Entry.parse entry ;
  value load ~{file} =
  let ic = open_in file in
  let old_input_file = Plexing.input_file.val in
  try do {
    Plexing.input_file.val := file ;
    let rv = input ic
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

end ;

module Json = PAHelper(struct type t = Json0.t ; value entry = json ; end) ;
module JsonEOI = PAHelper(struct type t = Json0.t ; value entry = json_eoi ; end) ;
module JsonOrEOI = PAHelper(struct type t = (option Json0.t) ; value entry = json_or_eoi ; end) ;
module JsonList = PAHelper(struct type t = list Json0.t ; value entry = json_list ; end) ;
module JsonListEOI = PAHelper(struct type t = list Json0.t ; value entry = json_list_eoi ; end) ;
