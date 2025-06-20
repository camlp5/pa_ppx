#ifdef PAPPX
(**pp -syntax camlp5o -package $(PAPACKAGES),$(PACKAGEPREFIX)import,$(PACKAGEPREFIX)here *)
#else
(**pp -package $(PPXPACKAGES) *)
#endif
open OUnit2

let here = [%here]

type position = [%import: Lexing.position] [@@deriving show]

let test_simplest ctxt =
#ifdef PAPPX
let pos = {
    Lexing.pos_fname = "test_here_original.ML";
    pos_lnum = 8;
    pos_cnum = 172;
    pos_bol = 161
  } in
#else
let pos = {
    Lexing.pos_fname = "test_here.ml";
    pos_lnum = 8;
    pos_cnum = 93;
    pos_bol = 82
  } in
#endif
 assert_equal ~printer:show_position pos here

let suite = "Test here" >::: [
    "test_simplest"   >:: test_simplest
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
