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

value expr_as_patt loc s =
  Pcaml.handle_patt_quotation loc ("expr@", s)
;

value patt_as_patt loc s =
  Pcaml.handle_patt_quotation loc ("patt@", s)
;

value generate_param_parser arg ty =
  let rec genrec = fun [
    <:ctyp:< int >> ->
      let p = expr_as_patt loc "$int:i$" in
    <:expr< fun [ $p$ → int_of_string i ] >>
  | <:ctyp:< bool >> ->
      let true_patt = expr_as_patt loc "True" in
      let false_patt = expr_as_patt loc "False" in
    <:expr< fun [ $true_patt$ → True | $false_patt$ → False ] >>
  | <:ctyp:< lident >> ->
      let p = expr_as_patt loc "$lid:lid$" in
    <:expr< fun [ $p$ →  lid ] >>
  | <:ctyp:< uident >> ->
      let p = expr_as_patt loc "$uid:uid$" in
    <:expr< fun [ $p$ →  uid ] >>
  | <:ctyp:< expr >> ->
    <:expr< fun [ e →  e ] >>
  | <:ctyp:< ctyp >> ->
      let p = expr_as_patt loc "[%typ: $type:t$]" in
    <:expr< fun [ $p$ →  t ] >>

  | <:ctyp:< longid >> ->
    <:expr< fun [ e → Pa_ppx_base.Ppxutil.longid_of_expr e  ] >>

  | <:ctyp:< $lid:id$ >> ->
      <:expr< $lid:params_fname arg id$ >>

  | <:ctyp:< $longid:li$ . $lid:id$ >> ->
      let e1 = expr_of_longid li in
      let e2 = <:expr< $lid:params_fname arg id$ >> in
      <:expr< $e1$ . $e2$ >>

  | <:ctyp:< { $list:ltl$ } >> as z ->
    let label_ty_notoutput_optional_default_computeds = List.map (fun (_, na, _, ty, al) ->
        let default = match List.find_map (fun a -> if attr_id a = "default" then Some (uv a) else None) (uv al) with [
          Some <:attribute_body< "default" $exp:d$ ; >> -> Some d
        | None -> None
        ] in
        let computed = match List.find_map (fun a -> if attr_id a = "computed" then Some (uv a) else None) (uv al) with [
          Some <:attribute_body< "computed" $exp:d$ ; >> -> Some d
        | None -> None
        ] in
        let notoutput = match List.find_map (fun a -> if attr_id a = "notoutput" then Some (uv a) else None) (uv al) with [
          Some <:attribute_body< "notoutput" >> -> True
        | None -> False
        ] in
        let (ty, optional) = match ty with [
            <:ctyp< option $t$ >> -> (t, True)
          | t -> (t, False)
          ] in
        (na, ty, notoutput, optional, default, computed)
      ) ltl in
    let consfields = List.map (fun (na, _, False, _, _, _) ->
        (<:patt< $lid:na$ >>, <:expr< $lid:na$ >>))
        label_ty_notoutput_optional_default_computeds in
    let consrhs = <:expr< { $list:consfields$ } >> in
    let one_field_binding (na, ty, notoutput, optional, default, computed) =
      match (optional, default, computed) with [
        (True, None, None) ->
        (<:patt< $lid:na$ >>, <:expr< match List.assoc $str:na$ __alist__ with [
                          x -> Some ($genrec ty$ x)
                        | exception Not_found -> None
                        ] >>, <:vala< [] >>)
      | (False, None, None) ->
        (<:patt< $lid:na$ >>, <:expr< match List.assoc $str:na$ __alist__ with [
                          x -> $genrec ty$ x
                        | exception Not_found ->
                          Ploc.raise loc
                            (Failure (Printf.sprintf "field %s is not optional" $str:na$)) 
                        ] >>, <:vala< [] >>)
      | (False, Some d, None) ->
        (<:patt< $lid:na$ >>, <:expr< match List.assoc $str:na$ __alist__ with [
                          x -> $genrec ty$ x
                        | exception Not_found -> $d$
                        ] >>, <:vala< [] >>)
      | (False, None, Some e) ->
        (<:patt< $lid:na$ >>, e, <:vala< [] >>)
      ]
    in
    let field_bindings = List.map one_field_binding label_ty_notoutput_optional_default_computeds in
    let full_rhs = List.fold_right (fun b e -> <:expr< let $list:[b]$ in $e$ >>) field_bindings consrhs in
    let recpat = expr_as_patt loc "{ $list:__lel__$ }" in
    let p = patt_as_patt loc "$lid:fld$" in
    <:expr< fun [ $recpat$ ->
            let __alist__ = List.map (fun [ ($p$, e) -> (fld, e) 
                                          | _ ->
                                            Ploc.raise loc
                                              (Failure "fields must be named by lidents")
                                          ]) __lel__ in
            $full_rhs$

                | e -> Ploc.raise (loc_of_expr e)
                         (Failure Fmt.(str "param did not match record-type:@ record-type: %s\n@ param: %a"
                                           $str:String.escaped (Pp_MLast.show_ctyp z)$ Pp_MLast.pp_expr e)) 
                ] >>

  | <:ctyp:< alist lident $rngty$ >> ->
    let lid_patt = patt_as_patt loc "$lid:k$" in
    let full_body = <:expr<
      List.map (fun ($lid_patt$, e) -> (k, $genrec rngty$ e)) __lel__
    >> in         
    let recpat = expr_as_patt loc "{ $list:__lel__$ }" in
    let unitpat = expr_as_patt loc "()" in
    <:expr< fun [ $unitpat$ -> []
                | $recpat$ -> $full_body$ ] >>

  | <:ctyp:< alist longid_lident $rngty$ >> ->
    let lid_patt = patt_as_patt loc "$lid:lid$" in
    let longlid_patt = patt_as_patt loc "$longid:li$ . $lid:lid$" in
    let full_body = <:expr<
      List.map (fun [ ($lid_patt$, e) -> ((None, Ploc.VaVal lid), $genrec rngty$ e)
                    | ($longlid_patt$, e) -> ((Some (Ploc.VaVal li), Ploc.VaVal lid), $genrec rngty$ e)
                    ]) __lel__
    >> in
    let recpat = expr_as_patt loc "{ $list:__lel__$ }" in
    let unitpat = expr_as_patt loc "()" in
    <:expr< fun [ $unitpat$ -> []
                | $recpat$ -> $full_body$ ] >>

  | <:ctyp:< alist ctyp $rngty$ >> ->
    let ctyp_patt = patt_as_patt loc "[%typ: $type:t$]" in
    let full_body = <:expr<
      List.map (fun ($ctyp_patt$, e) -> (t, $genrec rngty$ e)) __lel__
    >> in         
    let recpat = expr_as_patt loc "{ $list:__lel__$ }" in
    let unitpat = expr_as_patt loc "()" in
    <:expr< fun [ $unitpat$ -> []
                | $recpat$ -> $full_body$ ] >>

  | <:ctyp:< list $ty$ >> ->
    <:expr< Pa_ppx_base.Ppxutil.convert_down_list_expr $genrec ty$ >>

  | <:ctyp:< ( $list:l$ ) >> ->
    let vars_types = List.mapi (fun i ty -> (Printf.sprintf "v_%d" i, ty)) l in
    let varantis = List.map (fun (v, _) -> Printf.sprintf "$%s$" v) vars_types in
    let tuplepatt = expr_as_patt loc (Printf.sprintf "(%s)" (String.concat ", " varantis)) in
    let l = List.map (fun (v, t) -> <:expr< $genrec t$ $lid:v$ >>) vars_types in
    <:expr< fun $tuplepatt$ -> ( $list:l$ ) >>

  | <:ctyp:< $t$ [@convert ( [%typ: $type:srct$], $convf$ );] >> ->
    <:expr< fun __x__ -> $convf$ ($genrec srct$ __x__) >>

  | t -> Ploc.raise (loc_of_ctyp t) (Failure Fmt.(str "generate_param_parser: unhandled type %a"
                                                    Pp_MLast.pp_ctyp t))
  ] in
  match genrec ty with [
    <:expr< fun [ $list:_$ ] >> as z -> z
  | z -> let loc = loc_of_expr z in <:expr< fun x -> $z$ x >>
  ]
;

value generate_param_binding arg td =
 let loc = loc_of_type_decl td in
 let name = td.tdNam |> uv |> snd |> uv in
 (<:patt< $lid:params_fname arg name$ >>, generate_param_parser arg td.tdDef, <:vala< [] >>)
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
; options = ["optional"; "formal_args"]
; default_options = let loc = Ploc.dummy in [
    ("optional", <:expr< False >>)
  ; ("formal_args", <:expr< () >>)]
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun arg e -> assert False)
; ctyp = (fun arg e -> assert False)
; str_item = str_item_gen_params
; sig_item = (fun arg e -> assert False)
})
;

