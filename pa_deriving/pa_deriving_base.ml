(**pp -syntax camlp5r *)
(* camlp5r *)
(* pa_deriving_base.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Surveil ;

module ParamMap(ARG : sig value arg_ctyp_f : Ploc.t -> ctyp -> ctyp ; end) = struct

type param_t =
  {
    type_id : string ;
    arg_id : string
  }
;

value type_id p = p.type_id ;
value arg_id p = p.arg_id ;

value param_ctyp ?{mono=False} loc p =
if mono then
  <:ctyp< $lid:type_id p$ >>
else
  <:ctyp< ' $type_id p$ >>
;

value arg_ctyp ?{mono=False} loc p =
  let param_type = param_ctyp ~{mono=mono} loc p in
  ARG.arg_ctyp_f loc param_type
;
value arg_expr loc p = <:expr< $lid:arg_id p$ >> ;
value arg_patt ?{naked=False} ?{mono=False} loc p =
  if naked then
    <:patt< $lid:arg_id p$ >>
  else
    let cty = arg_ctyp ~{mono=mono} loc p in
    <:patt< ( $lid:arg_id p$ : $cty$ ) >>
;

value find id l =
  match List.find_opt (fun p -> type_id p = id) l with [
    None -> raise Not_found
  | Some p -> p ]
;

type t = list param_t ;

value make msg loc params =
  List.mapi (fun i p ->
    match uv (fst p) with [
      None -> Ploc.raise loc (Failure (Printf.sprintf "cannot derive %s-functions for type decl with unnamed type-vars" msg))
    | Some na -> { type_id = na ; arg_id = Printf.sprintf "tp_%d" i }
    ]) params
;

value make_of_ids pvl =
  List.mapi (fun i v -> { type_id = v ; arg_id = Printf.sprintf "tp_%d" i}) pvl
;

value quantify_over_ctyp param_map fty =
  let loc = loc_of_ctyp fty in
  if param_map = [] then fty
  else <:ctyp< ! $list:(List.map type_id param_map)$ . $fty$ >>
;

value wrap_type_constraints loc param_map funs types =
  List.map (fun (fname, body) ->
    let fty = List.assoc fname types in
    let fty = quantify_over_ctyp param_map fty in
    let attrwarn39 = <:attribute_body< "ocaml.warning" "-39" ; >> in
    let attrwarn39 = <:vala< attrwarn39 >> in
    let attrwarn33 = <:attribute_body< "ocaml.warning" "-33" ; >> in
    let attrwarn33 = <:vala< attrwarn33 >> in
    (<:patt< ( $lid:fname$ : $fty$ ) >>, body, <:vala< [attrwarn39; attrwarn33] >>)) funs
;

end
;

value monomorphize_ctyp cty =
  let rec mrec = fun [
    <:ctyp:< ' $id$ >> -> <:ctyp< $lid:id$ >>
  | <:ctyp:< $t1$ $t2$ >> -> <:ctyp< $mrec t1$ $mrec t2$ >>
  | <:ctyp:< [= $list:l$ ] >> ->
    let l = List.map (fun [
          PvTag loc cid b tyl attrs -> PvTag loc cid b (vala_map (List.map mrec) tyl) attrs 
        | PvInh loc ty -> PvInh loc (mrec ty)
            ]) l in
    <:ctyp:< [= $list:l$ ] >>
  | <:ctyp:< [ $list:l$ ] >> ->
    let l = List.map (fun [
          (loc, na, tyvars, tl, rto, al) ->
          (loc, na, tyvars, vala_map (List.map mrec) tl, vala_map (Option.map mrec) rto, al)
            ]) l in
    <:ctyp:< [ $list:l$ ] >>
  | <:ctyp:< ( $list:l$ ) >> -> <:ctyp:< ( $list:List.map mrec l$ ) >>
  | <:ctyp:< { $list:ltl$ } >> ->
      let ltl = List.map (fun (loc, na, b, ty, al) -> (loc, na, b, mrec ty, al)) ltl in
      <:ctyp:< { $list:ltl$ } >>
  | <:ctyp:< $t1$ == $t2$ >> -> <:ctyp:< $mrec t1$ == $mrec t2$ >>
  | <:ctyp:< $t1$ -> $t2$ >> -> <:ctyp:< $mrec t1$ -> $mrec t2$ >>
  | ty -> ty
  ]
  in mrec cty
;

value is_type_abbreviation = fun [
  <:ctyp< [ $list:_$ ] >> -> False
| <:ctyp< { $list:_$ } >> -> False
| _ -> True
]
;

value type_params t =
  let acc = ref [] in
  let add1 tv = if not (List.mem tv acc.val) then Std.push acc tv else () in
  let rec brec = fun [
    <:ctyp< ' $tv$ >> -> add1 tv
  | <:ctyp< $a$ $b$ >> -> do { brec a; brec b }
  | <:ctyp< ( $list:l$ ) >> -> List.iter brec l
  | <:ctyp< [= $list:branches$ ] >> ->
    List.iter (fun [ PvTag _ _ _ tyl _ -> List.iter brec (uv tyl) | PvInh _ ty -> brec ty ])  branches
  | _ -> ()
  ] in do {
    brec t ; List.rev acc.val
  }
;

value extract_allowed_attribute_expr arg (piname, attrna) attrs =
  match try_find (fun a -> match uv a with [ <:attribute_body< $attrid:(_, id)$ $exp:e$ ; >>
                   when Some id = DC.allowed_attribute (DC.get arg) piname attrna ->
                   e
                 | _ -> failwith "extract_allowed_attribute_expr" ]) attrs with [
    e -> Some e
  | exception Failure _ -> None ]
;
