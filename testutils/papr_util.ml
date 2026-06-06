(**pp -syntax camlp5r *)
(* camlp5r *)
(* testutil.ml,v *)

open Printf;

value with_input_file fname f arg =
  let oinput_file = Pcaml.input_file.val in do {
    Pcaml.input_file.val := fname ;
    try let rv = f arg in do { Pcaml.input_file.val := oinput_file ; rv }
    with exc -> do {
      Pcaml.input_file.val := oinput_file ;
      raise exc
    }
  }
;

module PAPRGen(PB : Mlsyntax.PARSEBASESIG)(PP : Mlsyntax.PRINTERS) = struct
module PA = PB.Parsers ;

value print_location loc =
  let loc =
    if Ploc.file_name loc = "" then
      Ploc.make_loc PB.input_file.val 1 0 (0, 1) ""
    else loc
  in
  let fname = Ploc.file_name loc in
  let bp = Ploc.first_pos loc in
  let ep = Ploc.last_pos loc in
  if fname <> "-" then
    let line = Ploc.line_nb loc in
    let bol = Ploc.bol_pos loc in
    eprintf "%s"
      (Pcamlbase.string_of_loc fname line (bp - bol + 1) (ep - bol + 1))
  else
    eprintf "At location %d-%d\n" bp ep
;

value report_error exc = do {
  Format.set_formatter_out_channel stderr;
  Format.open_vbox 0;
  let exc =
    match exc with
    [ Ploc.Exc loc exc -> do { print_location loc; exc }
    | _ -> exc ]
  in
  Pcaml.report_error exc;
  Format.close_box ();
  Format.print_newline ();
};

value report_error_and_exit ?{exit=True} exc = do {
  report_error exc ;
  if exit then Stdlib.exit 2 else raise exc
};

value wrap_err ?{exit=True} f arg =
try f arg with exc -> report_error_and_exit ~{exit=exit} exc
;

value with_input_file fname f arg =
  let oinput_file = PB.input_file.val in do {
    PB.input_file.val := fname ;
    try let rv = f arg in do { PB.input_file.val := oinput_file ; rv }
    with exc -> do {
      PB.input_file.val := oinput_file ;
      raise exc
    }
  }
;

module Implem = struct
value pa0 ~{parse_only} x =
  if parse_only then
    Grammar.Entry.parse PA.implem x
  else
    Pcaml.ParseBase.parse_implem x
;
value pa ?{parse_only=False} ?{input_file="-"} strm =
  let (ast, _) = with_input_file input_file (pa0 ~{parse_only}) strm in ast ;
value pa1 ?{parse_only=False} ?{parse_only=False} ?{input_file="-"} s =
  let ast = pa ~{parse_only} ~{input_file=input_file} (Stream.of_string s) in ast ;
value pa_all ?{parse_only=False} s =
  let strm = Stream.of_string s in
  let rec pall = parser [
    [: x = pa ~{parse_only} ; strm :] ->
    if x = [] then [] else
      x @ (pall strm)
  | [: :] -> [] ] in
  pall strm
;

value pr l = do {
  let sep = match Pcaml.inter_phrases.val with [ None -> "" | Some s -> s ] in
  let b = Buffer.create 23 in
    List.iter (fun (ast, _) -> 
      let s = Eprinter.apply PP.pr_str_item Pprintf.empty_pc ast in do {
        Buffer.add_string b s ;
        Buffer.add_string b sep ;
      }) l ;
    Buffer.contents b
}
;
value to_official x =
  x |> List.map fst |> Ast2pt.implem "<stdin>";
end;

module Interf = struct
value pa0 ~{parse_only} x =
  if parse_only then
    Grammar.Entry.parse PA.interf x
  else
    Pcaml.ParseBase.parse_interf x
;
value pa ?{parse_only=False} ?{input_file="-"} strm =
  let (ast, _) = with_input_file input_file (pa0 ~{parse_only}) strm in ast ;
value pa1 ?{parse_only=False} ?{parse_only=False} ?{input_file="-"} s =
  let ast = pa ~{parse_only} ~{input_file=input_file} (Stream.of_string s) in ast ;
value pa_all ?{parse_only=False} s =
  let strm = Stream.of_string s in
  let rec pall = parser [
    [: x = pa ~{parse_only} ; strm :] ->
    if x = [] then [] else
      x @ (pall strm)
  | [: :] -> [] ] in
  pall strm
;

value pr l = do {
  let sep = match Pcaml.inter_phrases.val with [ None -> "" | Some s -> s ] in
  let b = Buffer.create 23 in
    List.iter (fun (ast, _) -> 
      let s = Eprinter.apply PP.pr_sig_item Pprintf.empty_pc ast in do {
        Buffer.add_string b s ;
        Buffer.add_string b sep ;
      }) l ;
    Buffer.contents b
}
;
value to_official x =
  x |> List.map fst |> Ast2pt.interf "<stdin>";
end;
value both_pa1 = ((fun x -> Implem.pa1 x), (fun x -> Interf.pa1 x)) ;
value both_pr = ((fun x -> Implem.pr x), (fun x -> Interf.pr x)) ;
end;

module PAPR = PAPRGen(Pcaml.ParseBase)(Pcaml.PrintBase.Printers) ;

value with_buffer_formatter f arg = do {
  let b = Buffer.create 23 in
  let bfmt = Format.formatter_of_buffer b in
  f bfmt arg ;
  Format.pp_print_flush bfmt () ;
  Buffer.contents b
}
;

module Official = struct

module Implem = struct
value pa s =
  let lb = Lexing.from_string s in
  Parse.implementation lb
;
value pr st =
  Pprintast.string_of_structure st
;
end ;

module Interf = struct
value pa s =
  let lb = Lexing.from_string s in
  Parse.interface lb
;

value pr st =
  with_buffer_formatter Pprintast.signature st
;
end ;
value both_pa = (Implem.pa, Interf.pa) ;
value both_pr = (Implem.pr, Interf.pr) ;
end ;



(*
;;; Local Variables: ***
;;; mode:tuareg ***
;;; End: ***

*)
