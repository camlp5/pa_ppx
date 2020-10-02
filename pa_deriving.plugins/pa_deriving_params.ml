(* camlp5r *)
(* pa_deriving_params.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;
open Pa_deriving_base ;

value params_fname arg tyname =
  if tyname = "t" then "params"
  else "params_"^tyname
;

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc ty = assert False ; end) ;

value fmt_expression arg ?{coercion} param_map ty0 =
  let rec fmtrec ?{coercion} ?{attrmod=None} = fun [
    <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = params_fname arg lid in
  <:expr< $lid:fname$ >>

  | <:ctyp:< _ >> -> <:expr< fun x -> x >>
  | <:ctyp:< unit >> -> <:expr< fun x -> x >>
  | <:ctyp:< int >> -> <:expr< fun x -> x >>
  | <:ctyp:< int32 >> -> <:expr< fun x -> x >>
  | <:ctyp:< int64 >> -> <:expr< fun x -> x >>
  | <:ctyp:< nativeint >> -> <:expr< fun x -> x >>
  | <:ctyp:< float >> -> <:expr< fun x -> x >>
  | <:ctyp:< bool >> -> <:expr< fun x -> x >>
  | <:ctyp:< char >> -> <:expr< fun x -> x >>
  | <:ctyp:< string >> -> <:expr< fun x -> x >>
  | <:ctyp:< bytes >> -> <:expr< fun x -> x >>

  | <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun a -> List.map $fmt1$ a >>

  | <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< fun a -> Array.map $fmt1$ a >>

  | (<:ctyp:< ref $ty$ >> | <:ctyp:< Pervasives.ref $ty$ >>) ->
    let fmt1 = fmtrec ty in
    <:expr< fun a -> ref ($fmt1$ a.val) >>

  | <:ctyp:< lazy_t $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun [ lazy x ->  lazy ( $fmt1$ x ) ] >>

  | <:ctyp:< option $ty$ >> ->
    let fmt1 = fmtrec ty in
    <:expr< fun [
            None -> None
          | (Some a) -> Some ($fmt1$ a) ] >>

| (<:ctyp:< result $ty1$ $ty2$ >> | <:ctyp:< Result.result $ty1$ $ty2$ >>) ->
  <:expr< fun [
          (Result.Ok a) -> Result.Ok ($(fmtrec ty1)$ a)
        | (Result.Error a) -> Result.Error ($(fmtrec ty2)$ a)
      ] >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x | exception Not_found -> failwith "pa_deriving.params: unrecognized param-var in type-decl"
  ] in
  PM.arg_expr loc p

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when Some id = DC.allowed_attribute (DC.get arg) "params" "nobuiltin" ->
  fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec t

| <:ctyp:< $lid:lid$ >> ->
  let fname = params_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = params_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v,_) -> <:patt< $lid:v$ >>) vars_fmts in

    let fldmaps = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = <:expr< ( $list:fldmaps$ ) >> in

    <:expr< fun ( $list:var1pats$ ) -> $cmpexp$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    <:constructor:< $uid:cid$ of { $list:fields$ } $algattrs:_$ >> ->
    let (rec1pat, body) = fmt_record loc arg fields in

    let conspat = <:patt< ($uid:cid$ $rec1pat$) >> in
    (conspat, <:vala< None >>, <:expr< $uid:cid$ $body$ >>)

  | <:constructor:< $uid:cid$ of $list:tyl$ $algattrs:_$ >> ->
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> var1pats in    
    let fldmaps = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = Expr.applist <:expr< $uid:cid$ >> fldmaps in

    (conspat, <:vala< None >>, cmpexp)

  | (_, _, _, <:vala< Some _ >>, _) -> assert False
  ]) l in
  <:expr< fun [ $list:branches$ ] >>


| <:ctyp:< [= $list:l$ ] >> as z ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl _ ->
    let cid = uv cid in
    let tyl = uv tyl in
    let vars_fmts = List.mapi (fun i ty ->
        (Printf.sprintf "a_%d" i, fmtrec ty)) tyl in

    let var1pats = List.map (fun (v, _) -> <:patt< $lid:v$ >>) vars_fmts in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< ` $cid$ >> var1pats in

    let fldmaps = List.map (fun (v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) vars_fmts in
    let cmpexp = Expr.applist <:expr< ` $cid$  >> fldmaps in
    let cmpexp = <:expr< ( $cmpexp$ :> $z$ ) >> in

    (conspat, <:vala< None >>, cmpexp)

  | PvInh _ ty ->
    let lili = match fst (Ctyp.unapplist ty) with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some <:vala< li >>, lid)
    ] in
    let conspat = <:patt< ( # $lilongid:lili$ as a ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< ( ( $fmtf$ a ) :> $z$ ) >>)
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

  | <:ctyp:< { $list:fields$ } >> ->
  let (rec1pat, body) = fmt_record loc arg fields in
  let rec1pat = match coercion with [ None -> rec1pat | Some ty -> <:patt< ( $rec1pat$ : $ty$ ) >> ] in
  <:expr< fun $rec1pat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_params.fmt_expression"
  ]
  and fmt_record loc arg fields = 
  let labels_vars_fmts = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        (fname, Printf.sprintf "a_%s" fname, fmtrec ty)) fields in

  let v1_pl = List.map (fun (f, v,  _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_fmts in
  let v1pat = <:patt< { $list:v1_pl$ } >> in
  let fldmaps = List.map (fun (v, v1, fmtf) -> <:expr< $fmtf$ $lid:v1$ >>) labels_vars_fmts in
  let flds = List.map2 (fun (f,_) e -> (f,e)) v1_pl fldmaps in 
  let cmpexp = <:expr< { $list:flds$ } >>
  in
  (v1pat, cmpexp)
 in fmtrec ?{coercion=coercion} ty0
;

value fmt_top arg ~{coercion} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  fmt_expression arg ~{coercion=coercion} params t2
| t -> fmt_expression arg ~{coercion=coercion} params t
]
;

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "params" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let coercion =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    monomorphize_ctyp (Ctyp.applist ty paramtys) in
  let eqfname = params_fname arg tyname in
  let e = fmt_top arg ~{coercion=coercion} param_map ty in

  let paramfun_patts = List.map (PM.arg_patt ~{naked=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $lid:PM.type_id p$) >>) param_map in
  [(eqfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
      <:expr< fun arg -> $e$ arg >>)]
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let param_map = PM.make "params" loc (uv td.tdPrm) in
  let l = str_item_top_funs arg td in
  List.map (fun (fname, body) ->
      let attrwarn39 = <:attribute_body< "ocaml.warning" "-39" ; >> in
      let attrwarn39 = <:vala< attrwarn39 >> in
      (<:patt< $lid:fname$ >>, body, <:vala< [attrwarn39] >>)) l
;

value str_item_gen_params0 arg td =
  str_item_funs arg td
;

value generate_param_parser arg ty =
  let rec genrec = fun [
  ] in
  match genrec ty with [
    <:expr< fun [ $list:_$ ] >> as z -> z
  | z -> let loc = loc_of_expr z in <:expr< fun x -> $z$ x >>
  ]
;

value str_item_gen_params name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
  let l = List.concat (List.map (str_item_gen_params0 arg) tdl) in
  <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "params"
; alternates = []
; options = ["optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"]
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_params
; sig_item = (fun arg e -> assert False)
})
;

