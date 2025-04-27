(**pp -syntax camlp5o -package $(PAPACKAGES) *)
open OUnit2
open Pa_ppx_testutils

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let good str =
  let sexplib_sexp = Sexplib.Sexp.of_string str in
  let located_sexp = Pa_ppx_located_sexp.Sexp.of_string str in
  assert_equal ~msg:str (Pa_ppx_located_sexp.Sexp.to_sexplib_sexp located_sexp) sexplib_sexp

let bad ~sexplib_msg ~msg str =
  Testutil.assert_raises_exn_pattern sexplib_msg (fun _ -> Sexplib.Sexp.of_string str) ;
  Testutil.assert_raises_exn_pattern msg (fun _ -> Pa_ppx_located_sexp.Sexp.of_string str) ;
  ()

let test_good ctxt =
  ()
  ; good "()"
  ; good "a"
  ; good "(a)"
  ; good "(a())"
  ; good "foo"
  ; good "foo\n"
  ; good "foo;"
  ; good "foo #;()"
  ; good "foo #|blah|#"
  ; good "foo #|blah|#\n"
  ; good "foo; blah"
  ; good "foo; blah\n"
  ; good "foo; blah\n"
    (* multiple sexps *)
  ; bad ~sexplib_msg:"got multiple S-expressions" ~msg:"EOI expected" "foo bar"
    (* unterminated block comment *)
  ; bad ~sexplib_msg:"S-expression followed by data" ~msg:"unterminated block comment" "foo #| bar"
    (* unterminated sexp *)
  ; bad ~sexplib_msg:"S-expression followed by data" ~msg:"EOI expected" "foo ("
  ; good "#;() ( #;() ) #;()"


let suite = "Test located_sexp" >::: [
    "test_good"   >:: test_good
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
