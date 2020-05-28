(* camlp5r *)
(* pa_here.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

#load "pa_extend.cmo";
#load "q_MLast.cmo";
#load "pa_macro.cmo";
#load "pa_macro_gram.cmo";
#load "pa_extfun.cmo";

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value expr_to_longident eli =
  let rec erec = fun [
    <:expr:< $uid:s$ >> -> <:longident< $uid:s$ >>
  | <:expr:< $e1$ . $uid:s$ >> -> <:longident< $longid:erec e1$ . $uid:s$ >>
  | <:expr:< $e1$ . $e2$ >> ->
    match e2 with [
      <:expr:< $e2$ . $e3$ >> ->
        let e1 = <:expr< $e1$ . $e2$ >> in
        erec <:expr< $e1$ . $e3$ >>
      | _ -> Ploc.raise loc (Failure "expr_to_longident: bad expr")
    ]
  ]
  in erec eli
;
value is_rebind_to_attribute (attr : attribute) = attr_id attr = "rebind_to" ;

value rebind_extension_constructor arg = fun [
   EcTuple (loc, ci, _, None, attrs) as z
    when List.exists is_rebind_to_attribute (uv attrs) ->
    let (rebind_attrs, others) = filter_split is_rebind_to_attribute (uv attrs) in
    let eli = match List.map uv rebind_attrs  with [
      [ <:attribute_body:< rebind_to $exp:eli$ ; >> ] -> eli
    | _ -> Ploc.raise loc (Failure "rebind_extension_constructor: bad rebind_to attribute")
    ] in
    <:extension_constructor< $_uid:ci$ = $longid:expr_to_longident eli$ $algattrs:others$ >>
 ]
;

type scratchdata_t += [ Pa_rebindto of bool ] ;
value mk b = Pa_rebindto b ;
value init ctxt b = Ctxt.init_scratchdata ctxt "rebindto" (mk b) ;
value update ctxt b = Ctxt.update_scratchdata ctxt "rebindto" (mk b) ;
value get ctxt = match Ctxt.scratchdata ctxt "rebindto" with [
  Pa_rebindto b -> b
| _ -> failwith "Pa_rebindto: internal error in Ctxt.scratchdata"
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            extension_constructor = extfun ef.extension_constructor with [
    <:extension_constructor:< $uid:ci$ of $list:_$ $algattrs:attrs$ >> as z
    when List.exists is_rebind_to_attribute attrs ->
    fun arg ->
      if get arg then
        Some (rebind_extension_constructor arg z)
      else None
  | <:extension_constructor:< $uid:ci$  $algattrs:attrs$ >> as z
    when List.exists is_rebind_to_attribute attrs ->
    fun arg ->
      if get arg then
        Some (rebind_extension_constructor arg z)
      else None
  ] } in

let ef = EF.{ (ef) with
  implem = extfun ef.implem with [
    z ->
      fun arg ->
        let arg = init arg True in
        Some (Pa_passthru.implem0 arg z)
  ] } in

let ef = EF.{ (ef) with
  interf = extfun ef.interf with [
    z ->
      fun arg ->
        let arg = init arg False in
        Some (Pa_passthru.interf0 arg z)
  ] } in

let ef = EF.{ (ef) with
  signature = extfun ef.signature with [
    z ->
      fun arg ->
        let arg = update arg False in
        Some (Pa_passthru.signature0 arg z)
  ] } in

  Pa_passthru.(install { name = "pa_rebindto"; ef =  ef ; pass = None ; before = [] ; after = ["pa_deriving"] })
;

install();
