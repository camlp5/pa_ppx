#ifdef PAPPX
(**pp -syntax camlp5o -package $(PAPACKAGES),$(PACKAGEPREFIX)import,$(PACKAGEPREFIX)here -ppopt -pa_passthru-debug *)
#else
(**pp -package $(PPXPACKAGES) *)
#endif
open OUnit2

let here = [%here]

type position = [%import: Lexing.position] [@@deriving show]

let test_simplest ctxt =
#ifdef PAPPX
let pos = {
    Lexing.pos_fname = "test_here.ML";
    pos_lnum = 8;
    pos_cnum = 180;
    pos_bol = 169
  } in
#else
let pos = {
    Lexing.pos_fname = "test_here.ML";
    pos_lnum = 8;
    pos_cnum = 95;
    pos_bol = 84
  } in
#endif
 assert_equal ~printer:show_position pos here

#ifdef PAPPX
let wrapped_string = [%here_string "foo"]
let pos = {
    Lexing.pos_fname = "test_here.ML"
  ; pos_lnum = 31
  ; pos_bol = 499
  ; pos_cnum = 535
  }
let test_wrapped ctxt =
  assert_equal ~printer:show_position pos (fst wrapped_string)

#else
let test_wrapped ctxt = ()
#endif                     

let suite = "Test here" >::: [
      "test_simplest"   >:: test_simplest
    ; "test_wrapped"   >:: test_wrapped
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
