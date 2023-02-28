(**pp -syntax camlp5o -package pa_ppx_testutils,pa_ppx_base.link *)
open OUnit2
open Pa_ppx_testutils
open Papr_util

Pa_ppx_base.Pa_passthru.debug := true ;;

let contains_regexp restr s =
  match Str.(search_forward (regexp restr)  s 0) with
    _ -> true
  | exception Not_found -> false
;;

let test_simple ctxt =
  assert_equal true (contains_regexp "Lexing" ({foo| [%here] |foo} |> PAPR.Implem.pa1 |> PAPR.Implem.pr))

let suite = "Test linked PPX rewriter" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

