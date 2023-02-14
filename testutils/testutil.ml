(** -syntax camlp5r *)
(* camlp5r *)
(* testutil.ml,v *)

open Printf;

value map_stream f =
  let rec mrec = parser [
    [: `e ; strm :] -> [: `f e ; mrec strm :]
  | [: :] -> [: :]
  ] in mrec
;

value list_of_stream_eof eoftok strm =
  let rec lrec acc = parser [
    [: `e when e = eoftok :] -> List.rev [ e::acc ]
  | [: `e ; strm :] -> lrec [ e::acc ] strm
  | [: :] -> List.rev acc
  ] in
  lrec [] strm
;

value lex_string gram s =
  let lexer = Grammar.glexer gram in
  let (strm, _) = lexer.Plexing.tok_func (Stream.of_string s) in
  list_of_stream_eof ("EOI","") strm
;

value lex_string_loc gram s =
  let lexer = Grammar.glexer gram in
  let (strm, loct) = lexer.Plexing.tok_func (Stream.of_string s) in
  let rec tolist acc i =
    match Stream.peek strm with [
      None -> List.rev acc
    | Some (("EOI",_) as p) -> do {
      Stream.junk strm ;
      List.rev [("", p) :: acc]
    }
    | Some p -> do {
        Stream.junk strm ;
        let loc = Plexing.Locations.lookup loct i in
        let comm = Ploc.comment loc in
        tolist [(comm, p) :: acc] (i+1)
      }
   ]
  in
  tolist [] 0
;

value print_location loc =
  let loc =
    if Ploc.file_name loc = "" then
      Ploc.make_loc Pcaml.input_file.val 1 0 (0, 1) ""
    else loc
  in
  let fname = Ploc.file_name loc in
  let bp = Ploc.first_pos loc in
  let ep = Ploc.last_pos loc in
  if fname <> "-" then
    let line = Ploc.line_nb loc in
    let bol = Ploc.bol_pos loc in
    eprintf "%s"
      (Pcaml.string_of_loc fname line (bp - bol + 1) (ep - bol + 1))
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

open OUnitAssert ;

value assert_bool ?{printer} msg b =
  if not b then
    let msg0 = match printer with [ None -> "" | Some (f, arg) -> f arg ] in
    assert_failure (msg0^"\n"^msg)
  else ()
;

value assert_raises_exn_pred ?{msg} ?{exnmsg} exnpred (f: unit -> 'a) =
  let pexn =
    Printexc.to_string
  in
  let get_error_string () =
    let str =
      Format.sprintf
        "expected exception %s, but no exception was raised."
        (match exnmsg with [ None -> "<no message provided>" | Some msg -> msg ])
    in
      match msg with [
          None ->
            assert_failure str

        | Some s ->
            assert_failure (s^"\n"^str) ]
  in
    match raises f with [
       None ->
          assert_failure (get_error_string ())

      | Some e ->
          let msg = match msg with [ None -> "" | Some s -> s ] in
          assert_bool ~{printer=(pexn,e)} msg (exnpred e) ]
;


(*
;;; Local Variables: ***
;;; mode:tuareg ***
;;; End: ***

*)
