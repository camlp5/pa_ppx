(**pp -syntax camlp5r *)
(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value rewrite_expr arg = fun [
  <:expr:< [%here] >> ->
    let pos = start_position_of_loc loc in
    quote_position loc pos
| <:expr:< [%here $exp:e$;] >> ->
    let pos = start_position_of_loc (MLast.loc_of_expr e) in
    let posexp = quote_position loc pos in
    <:expr< ($posexp$, $e$) >>
| <:expr:< [%here_string $locstr:(sloc,s)$;] >> ->
    let pos = start_position_of_loc sloc in
    let posexp = quote_position loc pos in
    <:expr< ($posexp$, $locstr:(sloc,s)$) >>
| _ -> assert False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    (<:expr:< [%here] >> | <:expr:< [%here $exp:_$;] >> | <:expr:< [%here_string $locstr:_$;] >>) as z ->
    fun arg fallback ->
      Some (rewrite_expr arg z)
  ] } in
  Pa_passthru.(install { name = "pa_here"; ef =  ef ; pass = None ; before = [] ; after = [] })
;

install();
