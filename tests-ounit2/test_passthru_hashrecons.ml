(**pp -syntax camlp5o -package $(PAPACKAGES) *)
open OUnit2

open Pa_ppx_base
open HCPassthru

let test_equality (paf,f) na s =
  na >:: (fun ctxt ->
      let arg = paf s in
      let ef = Pa_passthru.EF.mk() in
      let ctxt = Pa_passthru.Ctxt.mk ef Ploc.dummy in
      assert_bool ("equality: "^s) (arg = f ctxt arg)
    )

let test_pointer_equality (paf,f) na s =
  na >:: (fun ctxt ->
      let arg = paf s in
      let ef = Pa_passthru.EF.mk() in
      let ctxt = Pa_passthru.Ctxt.mk ef Ploc.dummy in
      assert_bool ("pointer equality: "^s) (arg == f ctxt arg)
    )

let suite = "Test passthru hashrecons" >::: [
    test_pointer_equality (MLParsers.O.Pretty.String.expr, expr) "expr-0" "1"
    ; test_pointer_equality (MLParsers.O.Pretty.String.expr, expr) "expr-0b" "1"
    ; test_pointer_equality (MLParsers.O.Pretty.String.expr, expr) "expr-tup-0" "(1,2)"
    ; test_pointer_equality (MLParsers.O.Pretty.String.str_item, str_item) "si-0" "(1,2)"
    ; test_pointer_equality (MLParsers.O.Pretty.String.str_item, str_item) "si-typedecl-1" "type t = int"
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
