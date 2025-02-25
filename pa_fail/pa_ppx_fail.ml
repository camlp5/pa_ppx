(**pp -syntax camlp5r *)
(* camlp5r *)
(* pa_fail.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value rewrite_expr arg = fun [
  <:expr:< [%fail $str:s$ ;] >> ->
    raise (Ploc.Exc loc (Failure s))
| _ -> assert False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr< [%fail $str:s$ ;] >> as z ->
    fun arg fallback ->
      Some (rewrite_expr arg z)
  ] } in
  Pa_passthru.(install { name = "pa_fail"; ef =  ef ; pass = None ; before = [] ; after = [] })
;

install();
