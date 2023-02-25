(** -syntax camlp5r *)
(* pa_deriving_params.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;
open Surveil ;
open Pa_deriving_base ;

value params_fname arg tyname =
  if tyname = "t" then "params"
  else "params_"^tyname
;

value expr_as_patt loc s =
  Pcaml.handle_patt_quotation loc ("expr@", s)
;

value patt_as_patt loc s =
  Pcaml.handle_patt_quotation loc ("patt@", s)
;

value generate_param_parser_expression arg ty =
  let rec genrec = fun [
      <:ctyp< $_$ == $t$ >> -> genrec t
    | <:ctyp:< int >> ->
        let p = expr_as_patt loc "$int:i$" in
        <:expr< fun [ $p$ → int_of_string i
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be integer: %a" Pp_MLast.pp_expr e))
                ] >>

  | <:ctyp:< float >> ->
      let p = expr_as_patt loc "$flo:i$" in
    <:expr< fun [ $p$ → float_of_string i
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be float: %a" Pp_MLast.pp_expr e))
                ] >>

  | <:ctyp:< bool >> ->
      let true_patt = expr_as_patt loc "True" in
      let false_patt = expr_as_patt loc "False" in
    <:expr< fun [ $true_patt$ → True | $false_patt$ → False
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be bool: %a" Pp_MLast.pp_expr e))
                ] >>
  | <:ctyp:< lident >> ->
      let p = expr_as_patt loc "$lid:lid$" in
    <:expr< fun [ $p$ →  lid
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be LIDENT: %a" Pp_MLast.pp_expr e))
                ] >>
  | <:ctyp:< string >> ->
      let p = expr_as_patt loc "$str:s$" in
    <:expr< fun [ $p$ →  s
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be string: %a" Pp_MLast.pp_expr e))
                ] >>
  | <:ctyp:< uident >> ->
      let p = expr_as_patt loc "$uid:uid$" in
    <:expr< fun [ $p$ →  uid
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be UIDENT: %a" Pp_MLast.pp_expr e))
                ] >>
  | <:ctyp:< expr >> ->
    <:expr< fun [ e →  e ] >>
  | <:ctyp:< ctyp >> ->
      let p = expr_as_patt loc "[%typ: $type:t$]" in
    <:expr< fun [ $p$ →  t
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be of the form [%%typ: <type>]: %a" Pp_MLast.pp_expr e))
                ] >>

  | <:ctyp:< patt >> ->
      let p = expr_as_patt loc "[%patt ? $patt:q$]" in
    <:expr< fun [ $p$ →  q
                | e -> Ploc.raise (loc_of_expr e)
                            (Failure (Fmt.str "param should be of the form [%%patt? <patt>]: %a" Pp_MLast.pp_expr e))
                ] >>

  | <:ctyp:< longid >> ->
    <:expr< fun [ e → Pa_ppx_base.Ppxutil.longid_of_expr e  ] >>

  | <:ctyp:< $lid:id$ >> ->
      <:expr< $lid:params_fname arg id$ >>

  | <:ctyp:< $longid:li$ . $lid:id$ >> ->
      <:expr< $longid:li$ . $lid:params_fname arg id$ >>

  | <:ctyp:< { $list:ltl$ } >> as z ->
    let label_ty_optional_default_computeds = List.map (fun (_, na, _, ty, al) ->
        let fieldname = match List.find_map (fun a -> if attr_id a = "name" then Some (uv a) else None) (uv al) with [
          Some <:attribute_body< "name" $lid:fieldname$ ; >> -> fieldname
        | None -> na
        ] in
        let default = match List.find_map (fun a -> if attr_id a = "default" then Some (uv a) else None) (uv al) with [
          Some <:attribute_body< "default" $exp:d$ ; >> -> Some d
        | None -> None
        ] in
        let computed = match List.find_map (fun a -> if attr_id a = "computed" then Some (uv a) else None) (uv al) with [
          Some <:attribute_body< "computed" $exp:d$ ; >> -> Some d
        | None -> None
        ] in
        let (ty, optional) = match ty with [
            <:ctyp< option $t$ >> -> (t, True)
          | t -> (t, False)
          ] in
        ((na,fieldname), ty, optional, default, computed)
      ) ltl in
    let consfields = List.map (fun ((na,fieldname), _, _, _, _) ->
        (<:patt< $lid:na$ >>, <:expr< $lid:na$ >>))
        label_ty_optional_default_computeds in
    let consrhs = <:expr< { $list:consfields$ } >> in
    let one_field_binding ((na,fieldname), ty, optional, default, computed) =
      match (optional, default, computed) with [
        (_, _, Some e) ->
        (<:patt< $lid:na$ >>, e, <:vala< [] >>)

      | (True, None, None) ->
        (<:patt< $lid:na$ >>, <:expr< match List.assoc $str:fieldname$ __alist__ with [
                          x -> Some ($genrec ty$ x)
                        | exception Not_found -> None
                        ] >>, <:vala< [] >>)

      | (False, None, None) ->
        (<:patt< $lid:na$ >>, <:expr< match List.assoc $str:fieldname$ __alist__ with [
                          x -> $genrec ty$ x
                        | exception Not_found ->
                          Ploc.raise loc
                            (Failure (Printf.sprintf "field %s is not optional" $str:na$)) 
                        ] >>, <:vala< [] >>)

      | (False, Some d, None) ->
        (<:patt< $lid:na$ >>, <:expr< match List.assoc $str:fieldname$ __alist__ with [
                          x -> $genrec ty$ x
                        | exception Not_found -> $d$
                        ] >>, <:vala< [] >>)
      ]
    in
    let fieldnames = List.map (fun ((_,n), _, _, _, _) -> <:expr< $str:n$ >>) label_ty_optional_default_computeds in
    let fieldnames_expr = Ppxutil.convert_up_list_expr loc fieldnames in
    let field_bindings = List.map one_field_binding label_ty_optional_default_computeds in
    let full_rhs = List.fold_right (fun b e -> <:expr< let $list:[b]$ in $e$ >>) field_bindings consrhs in
    let recpat = expr_as_patt loc "{ $list:__lel__$ }" in
    let p = patt_as_patt loc "$lid:fld$" in
    <:expr< fun [ $recpat$ as z ->
            let __alist__ = List.map (fun [ ($p$, e) -> (fld, e) 
                                          | _ ->
                                            Ploc.raise loc
                                              (Failure "fields must be named by lidents")
                                          ]) __lel__ in
            let superfluous_fields = Pa_ppx_utils.Std.subtract (List.map fst __alist__) $fieldnames_expr$ in
            if [] = superfluous_fields then
              $full_rhs$
            else Ploc.raise (loc_of_expr z)
              (Failure Fmt.(str "superfluous (not-allowed) fields: %a" (list ~{sep=const string " "} string) superfluous_fields))
                | e -> Ploc.raise (loc_of_expr e)
                         (Failure Fmt.(str "param did not match record-type:@ record-type: %s\n@ param: %a"
                                           $str:String.escaped (Pp_MLast.show_ctyp z)$ Pp_MLast.pp_expr e)) 
                ] >>

  | <:ctyp:< alist lident $rngty$ >> as z ->
    let lid_patt = patt_as_patt loc "$lid:k$" in
    let full_body = <:expr<
      List.map (fun [ ($lid_patt$, e) -> (k, $genrec rngty$ e)
                    | (p, _) -> Ploc.raise (loc_of_patt p)
                            (Failure (Fmt.str "key should be of the form LIDENT: %a" Pp_MLast.pp_patt p))
                    ]) __lel__
    >> in         
    let recpat = expr_as_patt loc "{ $list:__lel__$ }" in
    let unitpat = expr_as_patt loc "()" in
    <:expr< fun [ $unitpat$ -> []
                | $recpat$ -> $full_body$
                | e -> Ploc.raise (loc_of_expr e)
                         (Failure Fmt.(str "param did not match alist-type:@ alist-type: %s\n@ param: %a"
                                           $str:String.escaped (Pp_MLast.show_ctyp z)$ Pp_MLast.pp_expr e)) 
                ] >>

  | <:ctyp:< alist longid_lident $rngty$ >> as z ->
    let lid_patt = patt_as_patt loc "$lid:lid$" in
    let longlid_patt = patt_as_patt loc "$longid:li$ . $lid:lid$" in
    let full_body = <:expr<
      List.map (fun [ ($lid_patt$, e) -> ((None, Ploc.VaVal lid), $genrec rngty$ e)
                    | ($longlid_patt$, e) -> ((Some (Ploc.VaVal li), Ploc.VaVal lid), $genrec rngty$ e)
                    | (p, _) -> Ploc.raise (loc_of_patt p)
                            (Failure (Fmt.str "key should be of the form [longid.]LIDENT: %a" Pp_MLast.pp_patt p))
                    ]) __lel__
    >> in
    let recpat = expr_as_patt loc "{ $list:__lel__$ }" in
    let unitpat = expr_as_patt loc "()" in
    <:expr< fun [ $unitpat$ -> []
                | $recpat$ -> $full_body$
                | e -> Ploc.raise (loc_of_expr e)
                         (Failure Fmt.(str "param did not match alist-type:@ alist-type: %s\n@ param: %a"
                                           $str:String.escaped (Pp_MLast.show_ctyp z)$ Pp_MLast.pp_expr e)) 
                ] >>

  | <:ctyp:< alist ctyp $rngty$ >> as z ->
     let pair_converter = genrec <:ctyp< (ctyp * $rngty$) >> in
    <:expr< fun __lel__ ->
      __lel__ |> Pa_ppx_base.Ppxutil.convert_down_list_expr $pair_converter$ |> 
      List.map (fun (ty, e) -> (ty, $genrec rngty$ e))
    >>

  | <:ctyp:< list $ty$ >> ->
    <:expr< Pa_ppx_base.Ppxutil.convert_down_list_expr $genrec ty$ >>

  | <:ctyp:< ne_list $ty$ >> ->
    <:expr< Pa_ppx_params_runtime.Runtime.convert_down_ne_list_expr $genrec ty$ >>

  | <:ctyp:< [ $list:branches$ ] >> as z ->
      let branches =
        branches |> List.map (fun [
            <:constructor< $uid:ci$ of $list:tl$ $algattrs:_$ >> ->
            let vars_types = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i, ty)) tl in
            let varantis = List.map (fun (v, _) -> Printf.sprintf "$%s$" v) vars_types in
            let conspatt = expr_as_patt loc (Printf.sprintf "%s %s" ci (String.concat " " varantis)) in
            let l = List.map (fun (v, t) -> <:expr< $genrec t$ $lid:v$ >>) vars_types in
            (conspatt, <:vala< None >>,
             Expr.applist <:expr< $uid:ci$ >> l)
          ]) in
      let branches = branches @ [
          (<:patt< e >>, <:vala< None >>,
           <:expr< Ploc.raise (loc_of_expr e)
                         (Failure Fmt.(str "param must be a constructor of type: %s\n@ param: %a"
                                           $str:String.escaped (Pp_MLast.show_ctyp z)$ Pp_MLast.pp_expr e)) 
           >>)
        ] in
      <:expr< fun [ $list:branches$ ] >>


  | <:ctyp:< ( $list:l$ ) >> as z ->
    let vars_types = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i, ty)) l in
    let varantis = List.map (fun (v, _) -> Printf.sprintf "$%s$" v) vars_types in
    let tuplepatt = expr_as_patt loc (Printf.sprintf "(%s)" (String.concat ", " varantis)) in
    let l = List.map (fun (v, t) -> <:expr< $genrec t$ $lid:v$ >>) vars_types in
    <:expr< fun [
                  $tuplepatt$ -> ( $list:l$ )
                | e -> Ploc.raise (loc_of_expr e)
                         (Failure Fmt.(str "param must be a tuple of type: %s\n@ param: %a"
                                           $str:String.escaped (Pp_MLast.show_ctyp z)$ Pp_MLast.pp_expr e)) 
                ] >>

  | <:ctyp:< $t$ [@convert ( [%typ: $type:srct$], $convf$ );] >> ->
    <:expr< fun __x__ -> $convf$ ($genrec srct$ __x__) >>

  | <:ctyp:< $t$ [@computed $exp:e$;] >> -> e

  | <:ctyp:< $t$ [@actual_args $exp:e$;] >> ->
      let args = convert_down_list_expr (fun x -> x) e in
      Expr.applist <:expr< $genrec t$ >> args

  | t -> Ploc.raise (loc_of_ctyp t) (Failure Fmt.(str "generate_param_parser_expression: unhandled type@ %a"
                                                    Pp_MLast.pp_ctyp t))
  ] in
  match genrec ty with [
    <:expr< fun [ $list:_$ ] >> as z -> z
  | z -> let loc = loc_of_expr z in <:expr< fun x -> $z$ x >>
  ]
;

value generate_param_parser arg name ty =
  let loc = loc_of_ctyp ty in
  let formal_args = match Ctxt.option arg "formal_args" with [
    <:expr< () >> -> []
  | <:expr< { $list:lel$ } >> -> lel
  | e -> Ploc.raise (loc_of_expr e)
      (Failure Fmt.(str "generate_param_parser: malformed \"formal_args\" option@ %a"
                      Pp_MLast.pp_expr e))
  ] in
  let listexp_opt = List.find_map (fun [
      (<:patt< $lid:name'$ >>, listexp) when name = name' -> Some listexp
    | _ -> None
    ]) formal_args in
  let formals = match listexp_opt with [
    None -> []
  | Some listexp ->
    convert_down_list_expr (fun [
        <:expr< $lid:lid$ >> -> lid
      | e -> Ploc.raise (loc_of_expr e)
          (Failure Fmt.(str "generate_param_parser: malformed \"formal_args\" list member@ %a"
                          Pp_MLast.pp_expr e))
      ]) listexp
  ] in
  let ppexp = generate_param_parser_expression arg ty in
  let validators = match Ctxt.option arg "validators" with [
    <:expr< { $list:lel$ } >> ->
    List.map (fun [
        (<:patt< $lid:n$ >>, e) -> (n,e)
      | _ -> Ploc.raise (loc_of_ctyp ty)
        (Failure Fmt.(str "malformed validator:@ %a" Pp_MLast.pp_ctyp ty))
      ]) lel
  | <:expr< () >> -> []
  | _ -> Ploc.raise (loc_of_ctyp ty)
      (Failure Fmt.(str "malformed validator:@ %a" Pp_MLast.pp_ctyp ty))
  | exception Not_found ->
      Ploc.raise (loc_of_ctyp ty)
        (Failure Fmt.(str "internal error: generate_param_parser: missing validator option:@ %a"
                        Pp_MLast.pp_ctyp ty))
  ] in
  let validator = match List.assoc name validators with [
    x -> x
  | exception Not_found -> <:expr< fun _ -> Result.Ok True >>
  ] in
  let ppexp = <:expr< fun __arg__ ->
                      let __e__ = $ppexp$ __arg__ in
                      match ($validator$ __e__) with [
                        Result.Error msg ->
                        Ploc.raise (MLast.loc_of_expr __arg__)
                          (Failure (Printf.sprintf "params failed validation check: %s" msg))
                      | Result.Ok False ->
                        Ploc.raise (MLast.loc_of_expr __arg__)
                          (Failure (Printf.sprintf "params failed validation check"))
                      | Result.Ok True -> __e__ ] >> in
  Expr.abstract_over
    (List.map (fun lid -> <:patt< $lid:lid$ >>) formals)
    ppexp
;

value generate_param_binding arg td =
 let loc = loc_of_type_decl td in
 let name = td.tdNam |> uv |> snd |> uv in
 (<:patt< $lid:params_fname arg name$ >>, generate_param_parser arg name td.tdDef, <:vala< [] >>)
;

value str_item_gen_params name arg = fun [
  <:str_item:< type $_flag:_$ $list:tdl$ >> ->
    let loc = loc_of_type_decl (List.hd tdl) in
  let l = List.map (generate_param_binding arg) tdl in
  <:str_item< value rec $list:l$ >>
| _ -> assert False ]
;

Pa_deriving.(Registry.add PI.{
  name = "params"
; alternates = []
; options = ["optional"; "formal_args";"validators"]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ; ("formal_args", <:expr< () >>)
  ; ("validators", <:expr< () >>)
  ]
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_params
; sig_item = (fun arg e -> assert False)
})
;

