(**pp -syntax camlp5r *)
(* camlp5r *)
(* pa_deriving_sexp.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;
open Surveil ;
open Pa_deriving_base ;


module Ctxt = struct
  include Pa_passthru.Ctxt ;

  value plugin_name ctxt = do {
    assert (List.mem_assoc "plugin_name" ctxt.options) ;
    match List.assoc "plugin_name" ctxt.options with [
      <:expr< $str:na$ >> -> na
    | _ -> assert False
    ]}
  ;
  value is_plugin_name ctxt s = (s = plugin_name ctxt) ;

  value is_strict ctxt =
    match List.assoc "strict" ctxt.options with [
      <:expr< True >> -> True
    | <:expr< False >> -> False
    | _ -> failwith "malformed option 'strict'"
    | exception Not_found -> assert False
    ]
  ;

  value is_exn ctxt =
    match List.assoc "exn" ctxt.options with [
      <:expr< True >> -> True
    | <:expr< False >> -> False
    | _ -> failwith "malformed option 'exn'"
    | exception Not_found -> assert False
    ]
  ;
end ;

value tuplepatt loc l = if List.length l = 1 then List.hd l else <:patt< ( $list:l$ ) >> ;
value tupleexpr loc l = if List.length l = 1 then List.hd l else <:expr< ( $list:l$ ) >> ;

module To = struct

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< $pty$ -> Sexplib0.Sexp.t >> ; end) ;

value to_sexp_fname arg tyname =
  (*if tyname = "t" then "sexp_of"
    else *)"sexp_of_"^tyname
;

type drop_instruction = [
    Builtin_eq
  | Poly
  | Eq
  | Compare
  | Sexp
  | Code of expr
  ]
;

value drop_default_instructions loc arg attrs =
  match extract_allowed_attribute_expr arg ("sexp", "default") attrs with [
      None -> None
    | Some d ->
       match (extract_allowed_attribute_expr arg ("sexp", "sexp_drop_default") attrs,
             extract_allowed_attribute_expr arg ("sexp", "sexp_drop_default.compare") attrs,
             extract_allowed_attribute_expr arg ("sexp", "sexp_drop_default.equal") attrs,
             extract_allowed_attribute_expr arg ("sexp", "sexp_drop_default.sexp") attrs) with [
           (None, None, None, None) -> None
         | (Some <:expr< () >>, None, None, None) -> Some (d, Builtin_eq)
         | (Some eqfun, None, None, None) -> Some (d, Code eqfun)
         | (None, Some <:expr:< () >>, None, None) -> Some (d, Compare)
         | (None, None, Some <:expr:< () >>, None) -> Some (d, Eq)
         | (None, None, None, Some <:expr:< () >>) -> Some (d, Sexp)
         | _ -> Ploc.raise loc (Failure "pa_deriving.sexp: unrecognized unrecognized or malformed drop instructions")
         ]
    ]
;

value to_expression arg ?{coercion} ~{msg} param_map ty0 =
  let rec fmtrec ?{coercion} ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = to_sexp_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< _ >> -> failwith "cannot derive sexp for type <<_>>"
| <:ctyp:< unit >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_unit >>
| <:ctyp:< int >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_int >>
| <:ctyp:< bool >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_bool >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_int32 >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_int64 >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< Sexplib0.Sexp_conv.sexp_of_string >>
| <:ctyp:< bytes >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_bytes >>
| <:ctyp:< char >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_char >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_nativeint >>
| <:ctyp:< float >> -> <:expr< Sexplib0.Sexp_conv.sexp_of_float >>

| <:ctyp:< Hashtbl.t >> ->
  <:expr< Sexplib0.Sexp_conv.sexp_of_hashtbl >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when Some id = DC.allowed_attribute (DC.get arg) "sexp" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when Some id = DC.allowed_attribute (DC.get arg) "sexp" "sexp_of" ->
    e

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.sexp_of_list $fmt1$ >>

| <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.sexp_of_array $fmt1$ >>

| (<:ctyp:< ref $ty$ >> | <:ctyp:< Stdlib.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.sexp_of_ref $fmt1$ >>

| <:ctyp:< option $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.sexp_of_option $fmt1$ >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x
  | exception Not_found ->
    Ploc.raise loc (Failure "pa_deriving.sexp: unrecognized param-var in type-decl")
  ] in
  PM.arg_expr loc p

| <:ctyp:< $lid:lid$ >> ->
  let fname = to_sexp_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = to_sexp_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    <:constructor:< $uid:cid$ of { $list:fields$ } $_algattrs:attrs$ >> ->
    let jscid = match extract_allowed_attribute_expr arg ("sexp", "name") (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let (recpat, body) = fmt_record loc arg fields in
    let body_liste = match body with [
          <:expr< Sexplib0.Sexp.List $liste$ >> -> liste
        | _ -> failwith "internal error handling inline records"
        ] in

    let conspat = <:patt< $uid:cid$ $recpat$ >> in
    (conspat, <:vala< None >>, <:expr< Sexplib0.Sexp.List [ (Sexplib0.Sexp.Atom $str:jscid$) :: $body_liste$ ] >>)

  | <:constructor:< $uid:cid$ of $list:tyl$ $_algattrs:attrs$ >> ->
    let jscid = match extract_allowed_attribute_expr arg ("sexp", "name") (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let fmts = List.map fmtrec tyl in
    let conspat = List.fold_left (fun p vp -> <:patt< $p$ $vp$ >>)
        <:patt< $uid:cid$ >> varpats in
    let consexp =
      match (vars, fmts) with [
          ([],[]) -> <:expr< Sexplib0.Sexp.Atom $str:jscid$ >>
        | _ ->
           let liste = List.fold_right2 (fun f v liste -> <:expr< [$f$ $lid:v$ :: $liste$] >>)
                         fmts vars <:expr< [] >> in
           <:expr< Sexplib0.Sexp.List [ (Sexplib0.Sexp.Atom $str:jscid$) :: $liste$ ] >>
        ] in
    (conspat, <:vala< None >>, consexp)

  | (_, _, _, _, <:vala< Some _ >>, _) -> assert False
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< [= $list:l$ ] >> ->
  let branches = List.map (fun [
    PvTag loc cid _ tyl attrs -> do {
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg ("sexp", "name") (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = uv tyl in
    assert (List.length tyl <= 1) ;
    let tyl = match tyl with [
      [] -> []
    | [<:ctyp< ( $list:l$ ) >>] -> l
    | [t] -> [t]
    | [_::_] -> assert False ] in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let conspat = if varpats = [] then
        <:patt< ` $cid$ >>
      else
        let tuplepat = tuplepatt loc varpats in
        <:patt< ` $cid$ $tuplepat$ >> in
    let e =
      match (vars, fmts) with [
          ([], []) ->
          <:expr< Sexplib0.Sexp.Atom $str:jscid$ >>
        | ([v],[f]) ->
           <:expr< Sexplib0.Sexp.List [Sexplib0.Sexp.Atom $str:jscid$ ; $f$ $lid:v$ ] >>
        | _ ->
           let liste = List.fold_right2 (fun f v liste -> <:expr< [$f$ $lid:v$ :: $liste$] >>)
                         fmts vars <:expr< [] >> in
           <:expr< Sexplib0.Sexp.List [Sexplib0.Sexp.Atom $str:jscid$ ; Sexplib0.Sexp.List $liste$] >>
        ] in
    (conspat, <:vala< None >>, e)
  }

  | PvInh _ ty ->
    let lili = match fst (Ctyp.unapplist ty) with [
      <:ctyp< $_lid:lid$ >> -> (None, lid)
    | <:ctyp< $longid:li$ . $_lid:lid$ >> -> (Some <:vala< li >>, lid)
    | [%unmatched_vala] -> failwith "fmt_expression-PvInh"
     ] in
    let conspat = <:patt< ( # $lilongid:lili$ as z ) >> in
    let fmtf = fmtrec ty in
    (conspat, <:vala< None >>, <:expr< ($fmtf$ z) >>)
  ]) l in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let liste = List.fold_right2 (fun f v liste -> <:expr< [$f$ $lid:v$ :: $liste$] >>)
        fmts vars <:expr< [] >> in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    <:expr< fun ($list:varpats$) -> Sexplib0.Sexp.List $liste$ >>

| <:ctyp:< { $list:fields$ } >> ->
  let (recpat, body) = fmt_record loc arg fields in
  let recpat = match coercion with [ None -> recpat | Some ty -> <:patt< ( $recpat$ : $ty$ ) >> ] in
  <:expr< fun $recpat$ -> $body$ >>

| [%unmatched_vala] -> failwith "pa_deriving_sexp.to_expression"
| ty ->
  Ploc.raise (loc_of_ctyp ty) (Failure (Printf.sprintf "pa_deriving_sexp.to_expression: %s" (Pp_MLast.show_ctyp ty)))
]
and fmt_record loc arg fields = 
  let labels_vars_tys_fmts_dropdefaults_jskeys = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        let attrs = snd(Ctyp.unwrap_attrs ty) in
        let drop_default = drop_default_instructions loc arg attrs in
        let key = extract_allowed_attribute_expr arg ("sexp", "key") attrs in
        let jskey = match key with [
          Some <:expr< $str:k$ >> -> k
        | Some _ -> failwith "@key attribute without string payload"
        | None -> fname ] in
        (fname, Printf.sprintf "v_%s" fname, ty, fmtrec ty, drop_default, jskey)) fields in

  let liste = List.fold_right (fun (f,v, ty,fmtf, drop_default, jskey) rhs ->
      match drop_default with [
        Some (d, Code eqcmpexp) -> <:expr< let fields = if $eqcmpexp$ $lid:v$ $d$ then fields
                           else [ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jskey$ ; $fmtf$ $lid:v$ ] :: fields ] in $rhs$ >>
      | Some (d, Sexp) -> <:expr< let fmtv = $fmtf$ $lid:v$ in
                          let fmtd = $fmtf$ $d$ in
                         let fields = if Sexplib.Sexp.equal fmtv fmtd then fields
                           else [ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jskey$ ; fmtv ] :: fields ] in $rhs$ >>
      | Some (d, Builtin_eq) -> <:expr< let fields = if $lid:v$ = $d$ then fields
                           else [ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jskey$ ; $fmtf$ $lid:v$ ] :: fields ] in $rhs$ >>
      | Some (d, Eq) -> <:expr< let fields = if [%eq: $type:ty$] $lid:v$ $d$ then fields
                           else [ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jskey$ ; $fmtf$ $lid:v$ ] :: fields ] in $rhs$ >>
      | Some (d, Compare) -> <:expr< let fields = if 0 = ([%ord: $type:ty$] $lid:v$ $d$) then fields
                           else [ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jskey$ ; $fmtf$ $lid:v$ ] :: fields ] in $rhs$ >>
      | _ -> <:expr< let fields = [ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jskey$ ; $fmtf$ $lid:v$ ] :: fields ] in $rhs$ >>
      ]) (List.rev labels_vars_tys_fmts_dropdefaults_jskeys) <:expr< fields >> in
  let liste = <:expr< let fields = [] in $liste$ >> in

  let pl = List.map (fun (f, v, _, _, _, _) -> (<:patt< $lid:f$ >>, <:patt< $lid:v$ >>)) labels_vars_tys_fmts_dropdefaults_jskeys in
  (<:patt< { $list:pl$ } >>, <:expr< Sexplib0.Sexp.List $liste$ >>)

in fmtrec ?{coercion=coercion} ty0
;


value fmt_to_top arg ~{coercion} ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  to_expression arg ~{coercion=coercion} ~{msg=msg} params t2
| t -> to_expression ~{coercion=coercion} arg ~{msg} params t
]
;

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let runtime_module = Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let param_map = PM.make "sexp" loc (uv td.tdPrm) in
  let tk = td.tdDef in
  let tyname = uv tyname in
  let ty =
    let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
    let ty = <:ctyp< $lid:tyname$ >> in
    (Ctyp.applist ty paramtys) in
  let coercion =
    monomorphize_ctyp ty in
  let to_sexpfname = to_sexp_fname arg tyname in
  let to_e = fmt_to_top arg ~{coercion=coercion} ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map tk in
  let to_e = <:expr< let open! $runtime_module$ in let open! Stdlib in $to_e$ >> in
  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $lid:PM.type_id p$) >>) param_map in
  let argexp =
    if uv td.tdPrv && is_type_abbreviation tk then
      <:expr< ( arg : $monomorphize_ctyp ty$ :> $monomorphize_ctyp tk$ ) >>
    else <:expr< arg >> in
  [(to_sexpfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
     <:expr< fun arg -> $to_e$ $argexp$ >>)]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "sexp" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let to_sexpfname = to_sexp_fname arg tyname in
  let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< $pty$ -> Sexplib0.Sexp.t >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let toftype = Ctyp.arrows_list loc argfmttys <:ctyp< $(Ctyp.applist ty paramtys)$ -> Sexplib0.Sexp.t >> in
  [(to_sexpfname, toftype)]
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let param_map = PM.make "sexp" loc (uv td.tdPrm) in
  let funs = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  PM.wrap_type_constraints loc param_map funs types
;

value extend_sig_items arg si = match si with [
  <:sig_item< type $_lid:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:sig_item< type $_lid:_$ $_list:_$ = $_$ == $_priv:_$ .. $_itemattrs:_$ >> 
 as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "sexp" loc (uv td.tdPrm) in
    let (to_sexpfname, toftype) = List.hd (sig_item_top_funs arg td) in
    let sil = [<:sig_item< value $lid:to_sexpfname$ : $toftype$>>] in
    let modname = Printf.sprintf "M_%s" to_sexpfname in
    let field_type = PM.quantify_over_ctyp param_map toftype in
    [ <:sig_item< [@@@ocaml.text "/*" ;] >> ;
      <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:to_sexpfname$ = { f: mutable  $field_type$ } ;
      value f : $lid:to_sexpfname$ ;
    end >> ;
      <:sig_item< [@@@ocaml.text "/*" ;] >> :: sil ]
| _ -> assert False
]
;

value rec extend_str_items arg si = match si with [
  <:str_item:< type $_lid:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $_lid:_$ $_list:_$ = $_$ == $_priv:_$ .. $_itemattrs:_$ >> 
 as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = PM.make "sexp" loc (uv td.tdPrm) in
    let (to_sexpfname, toftype) = List.hd (sig_item_top_funs arg td) in
    let modname = Printf.sprintf "M_%s" to_sexpfname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving sexp] is missing when extending the type " to_sexpfname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = PM.quantify_over_ctyp param_map toftype in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:to_sexpfname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< value $lid:to_sexpfname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (to_sexp_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some <:vala< li >> -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "sexp" loc params in
    let ec2gc = fun [
      EcTuple _ gc -> [gc]
    | EcRebind _ _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let e = to_expression arg ~{msg=String.escaped (Pp_MLast.show_longid_lident t)} param_map ty in
    let branches = match e with [
      <:expr< fun [ $list:branches$ ] >> -> branches
    | _ -> assert False
    ] in
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $lid:PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< z >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps@[ <:expr< z >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

  | <:str_item:< exception $excon:ec$ $itemattrs:attrs$ >> ->
    extend_str_items arg <:str_item:< type Pa_ppx_runtime_fat.Exceptions.t +=  [ $list:[ec]$ ] $itemattrs:attrs$ >>

| _ -> assert False
]
;

end
;

module Of = struct

type attrmod_t = [ Nobuiltin ] ;

module PM = ParamMap(struct value arg_ctyp_f loc pty = <:ctyp< Sexplib0.Sexp.t -> $pty$ >> ; end) ;

value of_sexp_fname arg tyname =
  (*if tyname = "t" then "of_sexp"
    else*) tyname^"_of_sexp"
;

value of_expression arg ~{msg} param_map ty0 =
  let rec fmtrec ?{attrmod=None} = fun [

  <:ctyp:< $lid:lid$ >> when attrmod = Some Nobuiltin ->
  let fname = of_sexp_fname arg lid in
  <:expr< $lid:fname$ >>

| <:ctyp:< unit >> -> <:expr< Sexplib0.Sexp_conv.unit_of_sexp >>
| <:ctyp:< int >> -> <:expr< Sexplib0.Sexp_conv.int_of_sexp >>
| <:ctyp:< bool >> -> <:expr< Sexplib0.Sexp_conv.bool_of_sexp >>
| <:ctyp:< int32 >> | <:ctyp:< Int32.t >> -> <:expr< Sexplib0.Sexp_conv.int32_of_sexp >>
| <:ctyp:< int64 >> | <:ctyp:< Int64.t >> -> <:expr< Sexplib0.Sexp_conv.int64_of_sexp >>
| (<:ctyp:< string >> | <:ctyp:< Stdlib.String.t >> | <:ctyp:< String.t >>) ->
  <:expr< Sexplib0.Sexp_conv.string_of_sexp >>
| <:ctyp:< bytes >> -> <:expr< Sexplib0.Sexp_conv.bytes_of_sexp >>
| <:ctyp:< char >> -> <:expr< Sexplib0.Sexp_conv.char_of_sexp >>
| <:ctyp:< nativeint >> | <:ctyp:< Nativeint.t >> -> <:expr< Sexplib0.Sexp_conv.nativeint_of_sexp >>
| <:ctyp:< float >> -> <:expr< Sexplib0.Sexp_conv.float_of_sexp >>

| <:ctyp:< Hashtbl.t >> ->
  <:expr< Sexplib0.Sexp_conv.hashtbl_of_sexp >>

| <:ctyp:< $t$ [@ $attrid:(_, id)$ ] >> when Some id = DC.allowed_attribute (DC.get arg) "sexp" "nobuiltin" ->
    fmtrec ~{attrmod=Some Nobuiltin} t

| <:ctyp:< $t$ [@ $attrid:(_, id)$ $exp:e$ ;] >> when Some id = DC.allowed_attribute (DC.get arg) "sexp" "of_sexp" ->
    e

| <:ctyp:< $t$ [@ $attribute:_$ ] >> -> fmtrec ~{attrmod=attrmod} t

| <:ctyp:< list $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.list_of_sexp $fmt1$ >>

| <:ctyp:< array $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.array_of_sexp $fmt1$ >>

| (<:ctyp:< ref $ty$ >> | <:ctyp:< Stdlib.ref $ty$ >>) ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.ref_of_sexp $fmt1$ >>

| <:ctyp:< option $ty$ >> ->
  let fmt1 = fmtrec ty in
  <:expr< Sexplib0.Sexp_conv.option_of_sexp $fmt1$ >>

| <:ctyp:< $t1$ $t2$ >> -> <:expr< $fmtrec t1$ $fmtrec t2$ >>

| <:ctyp:< '$i$ >> ->
  let p = match PM.find i param_map with [
    x -> x
  | exception Not_found ->
    Ploc.raise loc (Failure "pa_deriving.sexp: unrecognized param-var in type-decl")
  ] in
  PM.arg_expr loc p

| <:ctyp:< $lid:lid$ >> ->
  let fname = of_sexp_fname arg lid in
  <:expr< $lid:fname$ >>
| <:ctyp:< $longid:li$ . $lid:lid$ >> ->
  let fname = of_sexp_fname arg lid in
  Expr.prepend_longident li <:expr< $lid:fname$ >>

| <:ctyp:< [ $list:l$ ] >> ->
  let branches = List.map (fun [
    <:constructor:< $uid:cid$ of { $list:fields$ } $_algattrs:attrs$ >> ->
    let jscid = match extract_allowed_attribute_expr arg ("sexp", "name") (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let (recpat, body) = fmt_record ~{cid=Some cid} loc arg fields in
    let recpat_liste = match recpat with [
          <:patt< Sexplib0.Sexp.List xs >>-> <:patt< xs >>
        | _ -> failwith "internal error handling inline records"
        ] in
    let conspat = <:patt< Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jscid$ :: $recpat_liste$ ] >> in
    (conspat, <:vala< None >>, body)

  | <:constructor:< $uid:cid$ of $list:tyl$ $_algattrs:attrs$ >> ->
    let jscid = match extract_allowed_attribute_expr arg ("sexp", "name") (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in

    let conspat =
      match vars with [
          [] -> <:patt< Sexplib0.Sexp.Atom $str:jscid$ >>
        | _ ->
           let conspatvars = List.fold_right (fun v rhs -> <:patt< [ $lid:v$ :: $rhs$ ] >>)
                               vars <:patt< [] >> in
      <:patt< Sexplib0.Sexp.List [ (Sexplib0.Sexp.Atom $str:jscid$) :: $conspatvars$ ] >>
        ] in

    let varexps = List.map2 (fun fmt v -> <:expr< $fmt$ $lid:v$ >>) fmts vars in
    let consexp = Expr.applist <:expr< $uid:cid$ >> varexps in

    (conspat, <:vala< None >>, consexp)

  | (_, _, _, _, <:vala< Some _ >>, _) -> assert False
  ]) l in
  let catch_branch = (<:patt< _ >>, <:vala< None >>, <:expr< failwith $str:msg$ >>) in
  let branches = branches @ [catch_branch] in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< [= $list:l$ ] >> as ty0 -> 
  let ty0 = monomorphize_ctyp ty0 in
  let branches = List.map (fun [
    PvTag loc cid _ tyl attrs -> do {
    let cid = uv cid in
    let jscid = match extract_allowed_attribute_expr arg ("sexp", "name") (uv attrs) with [
      None -> cid | Some <:expr< $str:s$ >> -> s | _ -> failwith "@name with non-string argument"
    ] in
    let tyl = uv tyl in
    assert (List.length tyl <= 1) ;
    let tyl = match tyl with [
      [] -> []
    | [<:ctyp< ( $list:l$ ) >>] -> l
    | [t] -> [t]
    | [_::_] -> assert False ] in
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let fmts = List.map fmtrec tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let varexps = List.map2 (fun fmt v -> <:expr< $fmt$ $lid:v$ >>) fmts vars in
    let tup = tupleexpr loc varexps in
    let (conspat, consexp) =
      match varpats with [
          [] ->
          (<:patt< Sexplib0.Sexp.Atom $str:jscid$ >>, <:expr< ` $cid$ >>)                                    
        | [vp] ->
           (<:patt< Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jscid$ ; $vp$ ] >>,
            <:expr< ` $cid$ $tup$ >>)
        | _ ->
           let listpat = List.fold_right (fun vp listpat -> <:patt< [ $vp$ :: $listpat$ ] >>)
                           varpats <:patt< [] >> in
           (<:patt< Sexplib0.Sexp.List [(Sexplib0.Sexp.Atom $str:jscid$) ; Sexplib0.Sexp.List $listpat$] >>,
            <:expr< ` $cid$ $tup$ >>)
        ] in
    let consexp = <:expr< ( $consexp$ :> $ty0$ ) >> in
    Left (conspat, <:vala< None >>, consexp)
  }

  | PvInh _ ty ->
    let fmtf = fmtrec ty in
    Right fmtf
  ]) l in
  let (lefts,rights) = filter_split isLeft branches in
  let lefts = List.map (mustLeft "of_sexp") lefts in
  let rights = List.map (mustRight "of_sexp") rights in

  let righte = List.fold_right (fun fmtf rhs ->
    <:expr< try ( $fmtf$ sexp :> $ty0$ ) with [ Failure _ -> $rhs$ ] >>)
    rights <:expr< failwith $str:msg$ >> in

  let last_branch = (<:patt< sexp >>, <:vala< None >>,
                     righte) in

  let branches = lefts @ [ last_branch ] in
  <:expr< fun [ $list:branches$ ] >>

| <:ctyp:< ( $list:tyl$ ) >> ->
    let vars = List.mapi (fun n _ -> Printf.sprintf "v%d" n) tyl in
    let varpats = List.map (fun v -> <:patt< $lid:v$ >>) vars in
    let listpat = List.fold_right (fun v l -> <:patt< [$v$ :: $l$] >>) varpats <:patt< [] >> in
    let fmts = List.map fmtrec tyl in
    let unmarshe =
      let unmarshexps = List.map2 (fun fmte v -> <:expr< $fmte$ $lid:v$ >>) fmts vars in
      <:expr< ( $list:unmarshexps$ ) >> in
    <:expr< fun [ Sexplib0.Sexp.List $listpat$ -> $unmarshe$
                | _ -> failwith "wrong number of members in list" ] >>

| <:ctyp:< { $list:fields$ } >> ->
  let (recpat, body) = fmt_record ~{cid=None} loc arg fields in
  <:expr< fun [ $recpat$ -> $body$ | _ -> failwith $str:msg$ ] >>

| [%unmatched_vala] -> failwith "pa_deriving_sexp.of_expression"
| ty ->
  Ploc.raise (loc_of_ctyp ty) (Failure (Printf.sprintf "pa_deriving_sexp.of_expression: %s" (Pp_MLast.show_ctyp ty)))
]
and fmt_record ~{cid} loc arg fields = 
  let labels_vars_fmts_defaults_jskeys = List.map (fun (_, fname, _, ty, attrs) ->
        let ty = ctyp_wrap_attrs ty (uv attrs) in
        let attrs = snd(Ctyp.unwrap_attrs ty) in
        let default = extract_allowed_attribute_expr arg ("sexp", "default") attrs in
        let key = extract_allowed_attribute_expr arg ("sexp", "key") attrs in
        let jskey = match key with [
          Some <:expr< $str:k$ >> -> k
        | Some _ -> failwith "@key attribute without string payload"
        | None -> fname ] in
        (fname, Printf.sprintf "v_%s" fname, fmtrec ty, default, jskey)) fields in

  let varrow_except (i,iexp) =
    List.mapi (fun j (f,v,fmt,_,_) ->
        if i <> j then <:expr< $lid:v$ >> else iexp)
      labels_vars_fmts_defaults_jskeys in

  let branch1 i (f, v, fmt,_, jskey) =
    let l = varrow_except (i, <:expr< Result.Ok ( $fmt$ $lid:v$ ) >>) in
    let cons1exp = tupleexpr loc l in
    (<:patt< [ Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom $str:jskey$ ; $lid:v$ ] :: xs] >>, <:vala< None >>,
     <:expr< loop xs $cons1exp$ >>) in

  let branches = List.mapi branch1 labels_vars_fmts_defaults_jskeys in

  let finish_branch =
    let recexp =
      let lel = List.map (fun (f,v,_,_,_) -> (<:patt< $lid:f$ >>, <:expr< $lid:v$ >>)) labels_vars_fmts_defaults_jskeys in
      <:expr< { $list:lel$ } >> in
    let consexp = match cid with [ None -> recexp | Some cid -> <:expr< $uid:cid$ $recexp$ >> ] in
    let consexp = <:expr< Result.Ok $consexp$ >> in
    let e = List.fold_right (fun (_,v,_,_,_) rhs -> 
        <:expr< Rresult.R.bind $lid:v$ (fun $lid:v$ -> $rhs$) >>) labels_vars_fmts_defaults_jskeys consexp in
    (<:patt< [] >>, <:vala< None >>, e) in

  let catch_branch =
    if Ctxt.is_strict arg then
      (<:patt< [_ :: _] >>, <:vala< None >>, <:expr< failwith $str:msg$ >>)
  else
    let varrow = varrow_except (-1, <:expr< . >>) in
    let cons1exp = tupleexpr loc varrow in
    (<:patt< [_ :: xs] >>, <:vala< None >>, <:expr< loop xs $cons1exp$ >>) in

  let branches = branches @ [finish_branch; catch_branch] in

  let e = 
    let varpats = List.map (fun (_,v,_,_,_) -> <:patt< $lid:v$ >>) labels_vars_fmts_defaults_jskeys in
    let tuplevars = tuplepatt loc varpats in
    let initexps = List.map (fun (f,_,_,dflt,_) ->
        match dflt with [
          None ->
          let msg = msg^"."^f in
          <:expr< Result.Error $str:msg$ >>
        | Some d -> <:expr< Result.Ok $d$ >> ]) labels_vars_fmts_defaults_jskeys in
    let tupleinit = tupleexpr loc initexps in
    <:expr< let rec loop xs $tuplevars$ = match xs with [ $list:branches$ ]
            in match loop xs $tupleinit$ with [ Result.Ok r -> r | Result.Error msg -> failwith msg ] >> in

  (<:patt< Sexplib0.Sexp.List xs >>, e)

in fmtrec ty0
;

value fmt_of_top arg ~{msg} params = fun [
  <:ctyp< $t1$ == $_priv:_$ $t2$ >> ->
  of_expression arg ~{msg=msg} params t2
| t -> of_expression arg ~{msg=msg} params t
]
;

value str_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let runtime_module = Base.module_expr_runtime_module <:module_expr< Runtime >> in
  let param_map = PM.make "sexp" loc (uv td.tdPrm) in
  let ty = td.tdDef in
  let tyname = uv tyname in
  let of_sexpfname = of_sexp_fname arg tyname in
  let paramfun_patts = List.map (PM.arg_patt ~{mono=True} loc) param_map in
  let paramtype_patts = List.map (fun p -> <:patt< (type $lid:PM.type_id p$) >>) param_map in
  let body = fmt_of_top arg ~{msg=Printf.sprintf "%s.%s" (Ctxt.module_path_s arg) tyname} param_map ty in
  let e = 
    let of_e = <:expr< let open! $runtime_module$ in let open! Stdlib in $body$ >> in
    (of_sexpfname, Expr.abstract_over (paramtype_patts@paramfun_patts)
       <:expr< fun arg -> $of_e$ arg >>) in
  [e]
;

value sig_item_top_funs arg td =
  let (loc, tyname) = uv td.tdNam in
  let param_map = PM.make "sexp" loc (uv td.tdPrm) in
  let tyname = uv tyname in
  let of_sexpfname = of_sexp_fname arg tyname in
  let paramtys = List.map (fun p -> <:ctyp< ' $PM.type_id p$ >>) param_map in
  let argfmttys = List.map (fun pty -> <:ctyp< Sexplib0.Sexp.t -> $pty$ >>) paramtys in  
  let ty = <:ctyp< $lid:tyname$ >> in
  let offtype = Ctyp.arrows_list loc argfmttys <:ctyp< Sexplib0.Sexp.t -> $(Ctyp.applist ty paramtys)$ >> in
  let e = (of_sexpfname, offtype) in
  [e]
;

value sig_items arg td =
  let loc = loc_of_type_decl td in
  let mk1sig (fname, fty) = <:sig_item< value $lid:fname$ : $fty$>> in
  List.map mk1sig (sig_item_top_funs arg td)
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let param_map = PM.make "sexp" loc (uv td.tdPrm) in
  let funs = str_item_top_funs arg td in
  let types = sig_item_top_funs arg td in
  PM.wrap_type_constraints loc param_map funs types
;

value extend_sig_items arg si = match si with [
  <:sig_item:< type $_lid:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:sig_item:< type $_lid:_$ $_list:_$ = $_$ == $_priv:_$ .. $_itemattrs:_$ >> 
 as z ->
    let td = match z with [ <:sig_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let (loc, tyname) = uv td.tdNam in
    let param_map = PM.make "sexp" loc (uv td.tdPrm) in
    let sil = sig_items arg td in
    let (of_sexpfname, offtype) = List.hd (sig_item_top_funs arg td) in
    let modname = Printf.sprintf "M_%s" of_sexpfname in
    let field_type = PM.quantify_over_ctyp param_map offtype in
    [ <:sig_item< [@@@ocaml.text "/*" ;] >> ;
      <:sig_item< module $uid:modname$ :
    sig
      type nonrec $lid:of_sexpfname$ = { f: mutable  $field_type$ } ;
      value f : $lid:of_sexpfname$ ;
    end >> ;
      <:sig_item< [@@@ocaml.text "/*" ;] >> :: sil ]
| _ -> assert False
]
;

value rec extend_str_items arg si = match si with [
  <:str_item:< type $_lid:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >> 
| <:str_item:< type $_lid:_$ $_list:_$ = $_$ == $_priv:_$ .. $_itemattrs:_$ >> 
  as z ->
    let td = match z with [ <:str_item< type $_flag:_$ $list:tdl$ >> -> List.hd tdl | _ -> assert False ] in
    let param_map = PM.make "sexp" loc (uv td.tdPrm) in
    let (of_sexpfname, offtype) = List.hd (sig_item_top_funs arg td) in
    let modname = Printf.sprintf "M_%s" of_sexpfname in
    let msg1 = Printf.sprintf "%s: Maybe a [@@deriving sexp] is missing when extending the type " of_sexpfname in
    let msg2 = td.tdNam |> uv |> snd |> uv in

    let field_type = PM.quantify_over_ctyp param_map offtype in
    let fexp = <:expr< fun _ -> invalid_arg ($str:msg1$ ^ $str:msg2$) >> in
    let fexp = Expr.abstract_over (List.map (PM.arg_patt ~{mono=True} loc) param_map) fexp in
    let fexp = Expr.abstract_over (List.map (fun p -> <:patt< ( type $lid:PM.type_id p$ ) >>) param_map) fexp in
    [ <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< module $uid:modname$ =
    struct
      type nonrec $lid:of_sexpfname$ = { f: mutable  $field_type$ } ;
      value f = { f = $fexp$ } ;
    end >> ;
      <:str_item< [@@@ocaml.text "/*" ;] >> ;
      <:str_item< value $lid:of_sexpfname$ x = $uid:modname$ . f . $uid:modname$ . f x >>
    ]

| <:str_item:< type $lilongid:t$ $list:params$ += $_priv:_$ [ $list:ecs$ ] $_itemattrs:_$ >> ->
    let modname = Printf.sprintf "M_%s" (of_sexp_fname arg (uv (snd t))) in
    let modname = match fst t with [
      None -> <:longident< $uid:modname$ >>
    | Some <:vala< li >> -> <:longident< $longid:li$ . $uid:modname$ >>
    ] in
    let modname = module_expr_of_longident modname in
    let param_map = PM.make "sexp" loc params in
    let ec2gc = fun [
      EcTuple _ gc -> [gc]
    | EcRebind _ _ _ _ -> []
    ] in
    let gcl = List.concat (List.map ec2gc ecs) in
    let ty = <:ctyp< [ $list:gcl$ ] >> in
    let e = of_expression arg ~{msg=String.escaped (Pp_MLast.show_longid_lident t)} param_map ty in
    let branches = match e with [
      <:expr< fun [ $list:branches$ ] >> -> branches
    | _ -> assert False
    ] in
    (* remove the catch-branch *)
    let (_, branches) = sep_last branches in 
    let paramexps = List.map (PM.arg_expr loc) param_map in
    let parampats = List.map (PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $lid:PM.type_id p$) >>) param_map in
    let catch_branch = (<:patt< z >>, <:vala< None >>,
                        Expr.applist <:expr< fallback >> (paramexps @[ <:expr< z >> ])) in
    let branches = branches @ [ catch_branch ] in
    let e = <:expr< fun [ $list:branches$ ] >> in
    let e = Expr.abstract_over (paramtype_patts@parampats) e in
    [ <:str_item<
      let open $!:False$ $modname$ in
      let fallback = f . f in
      f.f := $e$ >> ]

  | <:str_item:< exception $excon:ec$ $itemattrs:attrs$ >> ->
    extend_str_items arg <:str_item:< type Pa_ppx_runtime_fat.Exceptions.t +=  [ $list:[ec]$ ] $itemattrs:attrs$ >>

| _ -> assert False
]
;

end
;

value str_item_funs arg td =
  (if Ctxt.is_plugin_name arg "sexp_of" || Ctxt.is_plugin_name arg "sexp" then
     To.str_item_funs arg td
   else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.str_item_funs arg td
   else [])
;

value sig_item_top_funs arg td =
  (if Ctxt.is_plugin_name arg "sexp_of" || Ctxt.is_plugin_name arg "sexp" then
    To.sig_item_top_funs arg td
  else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.sig_item_top_funs arg td
   else [])
;

value str_item_funs arg td =
  let loc = loc_of_type_decl td in
  let l = str_item_funs arg td in
  if l = [] then Ploc.raise loc (Failure "no functions generated") else l
;

value sig_items arg td =
  let loc = loc_of_type_decl td in
  let l = sig_item_top_funs arg td in
  List.map (fun (fname, ty) ->
      <:sig_item< value $lid:fname$ : $ty$>>) l
;

value str_item_gen_sexp0 arg td =
  str_item_funs arg td
;

value extend_sig_items arg td =
  (if Ctxt.is_plugin_name arg "sexp_of" || Ctxt.is_plugin_name arg "sexp" then
     To.extend_sig_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.extend_sig_items arg td
   else [])
;

value extend_str_items arg td =
  (if Ctxt.is_plugin_name arg "sexp_of" || Ctxt.is_plugin_name arg "sexp" then
     To.extend_str_items arg td
  else []) @
  (if Ctxt.is_plugin_name arg "of_sexp" || Ctxt.is_plugin_name arg "sexp" then
     Of.extend_str_items arg td
   else [])
;

value str_item_gen_sexp name arg = fun [
  <:str_item:< type $_lid:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:str_item:< type $_lid:_$ $_list:_$ = $_$ == $_priv:_$ .. $_itemattrs:_$ >> 
 as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< exception $excon:_$ $itemattrs:_$ >> as z ->
    let l = extend_str_items arg z in
    <:str_item< declare $list:l$ end >>

| <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (str_item_gen_sexp0 arg) tdl) in
    <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

value sig_item_gen_sexp name arg = fun [
  <:sig_item:< type $_lid:_$ $_list:_$ = $_priv:_$ .. $_itemattrs:_$ >>
| <:sig_item:< type $_lid:_$ $_list:_$ = $_$ == $_priv:_$ .. $_itemattrs:_$ >> 
 as z ->
    let l = extend_sig_items arg z in
    <:sig_item< declare $list:l$ end >>

| <:sig_item:< type $lilongid:_$ $_list:_$ += $_priv:_$ [ $list:_$ ] $_itemattrs:_$ >> as z ->
    <:sig_item< declare $list:[]$ end >>

| <:sig_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
    let l = List.concat (List.map (sig_items arg) tdl) in
    <:sig_item< declare $list:l$ end >>
| _ -> assert False ]
;

value expr_sexp arg = fun [
  <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "sexp_of" || id = "derive.sexp_of" ->
    let loc = loc_of_ctyp ty in
    let runtime_module = Base.module_expr_runtime_module <:module_expr< Runtime >> in
    let param_map = ty |> type_params |> To.PM.make_of_ids in
    let coercion = monomorphize_ctyp ty in
    let e = To.fmt_to_top arg ~{coercion=coercion} ~{msg=Printf.sprintf "%s.sexp_of"  (Ctxt.module_path_s arg)} param_map ty in
    let e = <:expr< let open! $runtime_module$ in let open! Stdlib in $e$ >> in
    let parampats = List.map (To.PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $lid:To.PM.type_id p$) >>) param_map in
    Expr.abstract_over (paramtype_patts@parampats) e

| <:expr:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_sexp" || id = "derive.of_sexp" ->
    let loc = loc_of_ctyp ty in
    let runtime_module = Base.module_expr_runtime_module <:module_expr< Runtime >> in
    let param_map = ty |> type_params |> Of.PM.make_of_ids in
    let e = Of.fmt_of_top ~{msg=Printf.sprintf "%s.of_sexp"  (Ctxt.module_path_s arg)} arg param_map ty in
    let e = <:expr< let open! $runtime_module$ in let open! Stdlib in $e$ >> in
    let parampats = List.map (Of.PM.arg_patt ~{mono=True} loc) param_map in
    let paramtype_patts = List.map (fun p -> <:patt< (type $lid:Of.PM.type_id p$) >>) param_map in
    Expr.abstract_over (paramtype_patts@parampats) e
| _ -> assert False ]
;

value ctyp_sexp arg = fun [
  <:ctyp:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "sexp_of" || id = "derive.sexp_of" ->
    let param_map = ty |> type_params |> To.PM.make_of_ids in
    let argfmttys = List.map (To.PM.arg_ctyp loc) param_map in  
    Ctyp.arrows_list loc argfmttys <:ctyp< $ty$ -> Sexplib0.Sexp.t >>

| <:ctyp:< [% $attrid:(_, id)$: $type:ty$ ] >> when id = "of_sexp" || id = "derive.of_sexp" ->
    let param_map = ty |> type_params |> Of.PM.make_of_ids in
    let argfmttys = List.map (Of.PM.arg_ctyp loc) param_map in  
    Ctyp.arrows_list loc argfmttys <:ctyp< Sexplib0.Sexp.t -> $ty$ >>

| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "sexp"
; alternates = ["of_sexp"; "sexp_of"; "sexp_poly"]
; options = ["optional"; "strict"; "exn"]
; default_options = let loc = Ploc.dummy in
    [ ("optional", <:expr< False >>); ("strict", <:expr< False >>); ("exn", <:expr< False >>) ]
; alg_attributes = ["nobuiltin"; "key"; "name"; "encoding"; "default"
                    ; "sexp_drop_default"
                    ; "sexp_drop_default.compare"
                    ; "sexp_drop_default.equal"
                    ; "sexp_drop_default.sexp"
                    ; "sexp_of"; "of_sexp"]
; expr_extensions = ["of_sexp"; "sexp_of"]
; ctyp_extensions = ["of_sexp"; "sexp_of"]
; expr = expr_sexp
; ctyp = ctyp_sexp
; str_item = str_item_gen_sexp
; sig_item = sig_item_gen_sexp
})
;

