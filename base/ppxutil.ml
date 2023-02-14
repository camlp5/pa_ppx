(** -syntax camlp5r *)
(* camlp5r *)
(* pa_passthru.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_utils;
open Asttools;
open MLast;

value uv = Pcaml.unvala ;

value with_buffer_formatter f arg = do {
  let b = Buffer.create 23 in
  let bfmt = Format.formatter_of_buffer b in
  f bfmt arg ;
  Format.pp_print_flush bfmt () ;
  Buffer.contents b
}
;

value duplicated (l : list string) =
  let l = List.sort Stdlib.compare l in
  let rec duprec = fun [
    [] -> False
  | [ h1 ; h2 :: acc ] when h1=h2 -> True
  | [ _ :: acc ] -> duprec acc
  ] in
  duprec l
;

value filter_split p l =
  let rec filtrec yes no = fun [
      [] -> (List.rev yes, List.rev no)
    | [x::l] -> if p x then filtrec [x::yes] no l else filtrec yes [x::no] l ]
  in filtrec [] [] l
;

value count p l = List.length (Std.filter p l) ;

value attr_id attr = snd (uv (fst (uv attr))) ;

value module_expr_of_longident li =
  let rec crec = fun [
    <:extended_longident:< $uid:m$ >> -> <:module_expr< $uid:m$ >>
  | <:extended_longident:< $longid:li$ . $uid:m$ >> -> <:module_expr< $crec li$ . $uid:m$ >>
  | <:extended_longident:< $longid:li1$ ( $longid:li2$ ) >> -> <:module_expr< $crec li1$ ( $crec li2$ ) >>
  ] in
  crec li
;
value longid_of_expr = fun [
  <:expr< $longid:li$ >> -> li
]
;

value expr_of_longid li =
  let loc = loc_of_longid li in
  <:expr< $longid:li$ >> ;

value convert_down_list_expr f e =
  let rec crec acc = fun [
    <:expr< [] >> -> List.rev acc
  | <:expr< [ $h$ :: $tl$ ] >> ->
    crec [ f h :: acc ] tl
  | _ -> Ploc.raise (loc_of_expr e) (Failure Fmt.(str "convert_down_list_expr: malformed list-expression %a"
                                                    Pp_MLast.pp_expr e))
  ] in
  crec [] e
;

value convert_up_list_expr loc el =
  List.fold_right (fun e rhs -> <:expr< [ $e$ :: $rhs$ ] >>) el <:expr< [] >>
;

module Env = struct
type t 'a = list (string * 'a) ;

value add loc rho id ty =
  if List.mem_assoc id rho then
    Ploc.raise loc (Failure "Ctyp.add_rho: adding same type-variable more than once")
  else [ (id, ty) :: rho ]
;

value append loc rho1 rho2 =
  List.fold_left (fun rho (id, ty) -> (add loc) rho id ty) rho1 rho2 ;
end
;

module Expr = struct

value print e =
  let e = Ast2pt.expr e in
  Pprintast.string_of_expression e ;

value to_string_list e =
  let rec srec = fun [
    <:expr< $longid:li$ >> -> Asttools.string_list_of_longident li
  | <:expr< $e$ . $lilongid:lili$ >> -> (srec e)@(Asttools.string_list_of_longident_lident lili)
  | <:expr< $lid:i$ >> -> [i]
  | e -> Ploc.raise (loc_of_expr e) (Failure "Expr.to_string_list: invalid expr")
  ]
  in srec e
;

value prepend_uid loc uid e =
  Asttools.expr_concat <:expr< $uid:uid$ >> e
;

value prepend_longident li e =
  let rec prerec li e = match li with [
    <:longident:< $uid:uid$ >> -> prepend_uid loc uid e
  | <:longident:< $longid:li$ . $uid:uid$ >> -> prerec li (prepend_uid loc uid e)
  | li -> Ploc.raise (loc_of_longid li) (Failure (Printf.sprintf "unexpected longid: %s" (Pp_MLast.show_longid li)))
  ] in
  prerec li e
;

value abstract_over l e =
  List.fold_right (fun p e -> let loc = loc_of_patt p in <:expr< fun $p$ -> $e$ >>) l e
;

value applist e el =
  List.fold_left (fun e arg -> let loc = loc_of_expr arg in <:expr< $e$ $arg$ >>) e el
;
value unapplist e =
  let rec unrec acc = fun [
    <:expr< $t$ $arg$ >> -> unrec [arg::acc] t
  | t -> (t,acc)
  ] in unrec [] e
;
value tuple loc l = if List.length l = 1 then List.hd l else <:expr< ( $list:l$ ) >> ;
end ;

module Patt = struct

value applist e el =
  List.fold_left (fun e arg -> let loc = loc_of_patt arg in <:patt< $e$ $arg$ >>) e el
;

value unapplist e =
  let rec unrec acc = fun [
    <:patt< $t$ $arg$ >> -> unrec [arg::acc] t
  | t -> (t,acc)
  ] in unrec [] e
;

value wrap_attrs ty al =
  let loc = loc_of_patt ty in
  List.fold_left (fun ty attr -> <:patt< $ty$  [@ $_attribute:attr$ ] >>)
    ty al
;


value unwrap_attrs e =
  let rec unrec acc = fun [
    <:patt< $t$  [@ $_attribute:attr$ ] >> -> unrec [attr::acc] t
  | t -> (t,acc)
  ] in unrec [] e
;
value tuple loc l = if List.length l = 1 then List.hd l else <:patt< ( $list:l$ ) >> ;
end ;

module Ctyp = struct

value print cty =
  let cty = Ast2pt.ctyp cty in
  with_buffer_formatter Pprintast.core_type cty ;

value arrows_list loc l ty =
  List.fold_right (fun argty ty -> <:ctyp< $argty$ -> $ty$ >>)
    l ty
;

value wrap_attrs ty al =
  let loc = loc_of_ctyp ty in
  List.fold_left (fun ty attr -> <:ctyp< $ty$  [@ $_attribute:attr$ ] >>)
    ty al
;


value unwrap_attrs e =
  let rec unrec acc = fun [
    <:ctyp< $t$  [@ $_attribute:attr$ ] >> -> unrec [attr::acc] t
  | t -> (t,acc)
  ] in unrec [] e
;

value applist e el =
  List.fold_left (fun e arg -> let loc = loc_of_ctyp arg in <:ctyp< $e$ $arg$ >>) e el
;

value unapplist e =
  let rec unrec acc = fun [
    <:ctyp< $t$ $arg$ >> -> unrec [arg::acc] t
  | t -> (t,acc)
  ] in unrec [] e
;

type rho = Env.t MLast.ctyp ;
value rec subst rho = fun [
  <:ctyp< ' $id$ >> when List.mem_assoc id rho -> List.assoc id rho
| <:ctyp:< $t1$ $t2$ >> -> <:ctyp< $subst rho t1$ $subst rho t2$ >>
| <:ctyp:< ( $list:l$ ) >> -> <:ctyp< ( $list:List.map (subst rho) l$ ) >>
| <:ctyp:< [ $list:l$ ] >> ->
  let l = List.map (fun [
      <:constructor:< $uid:s$ of $list:lt$ $rto:ot$ $_algattrs:x$ >> ->
      <:constructor< $uid:s$ of $list:List.map (subst rho) lt$
                     $rto:option_map (subst rho) ot$ $_algattrs:x$ >>
    ]) l in
  <:ctyp< [ $list:l$ ] >>
| <:ctyp:< { $list:l$ } >> ->
  let l = List.map (fun (a,b,c,ty,e) -> (a,b,c, subst rho ty,e)) l in
  <:ctyp< { $list:l$ } >>
| ( <:ctyp< $longid:_$ . $lid:_$ >>
  | <:ctyp< $lid:_$ >>
  ) as z -> z
| <:ctyp:< $t1$ -> $t2$ >> -> <:ctyp< $subst rho t1$ -> $subst rho t2$ >>
| z -> Ploc.raise (loc_of_ctyp z) (Failure Fmt.(str "Ctyp.subst: unhandled type: %a\n%!" Pp_MLast.pp_ctyp z))
]
;
value tuple loc l = if List.length l = 1 then List.hd l else <:ctyp< ( $list:l$ ) >> ;
end ;

module Longid = struct
  
value to_string_list li =
  let rec lirec = fun [
    <:longident< $uid:uid$ >> -> [uid]
  | <:longident< $longid:li$ . $uid:uid$ >> -> (lirec li) @ [uid]
  | <:extended_longident< $longid:_$ ( $longid:_$ ) >> ->
    failwith "longid_to_string_list: LiApp not allowed here"
  | <:longident< $_uid:_$ >> | <:longident< $longid:_$ . $_uid:_$ >> ->
    failwith "[internal error] longid_to_string_list: called with longid containing placeholders"
  ] in
  lirec li
;
end
;

value rec is_poly_variant t =
  let (t,_) = Ctyp.unwrap_attrs t in
  match t with [
    <:ctyp< [ = $list:_$ ] >> -> True
  | <:ctyp< [ > $list:_$ ] >> -> True
  | <:ctyp< [ < $list:_$ ] >> -> True
  | _ -> False ] 
;

value rec is_generative_type t =
  let (t,_) = Ctyp.unwrap_attrs t in
  match t with [
    <:ctyp< [ $list:_$ ] >> -> True
  | <:ctyp< { $list:_$ } >> -> True
  | _ -> False ] 
;

value ocaml_location (fname, lnum, bolp, lnuml, bolpl, bp, ep) =
    let loc_at n lnum bolp =
      {Lexing.pos_fname = if lnum = -1 then "" else fname;
       Lexing.pos_lnum = lnum; Lexing.pos_bol = bolp; Lexing.pos_cnum = n}
    in
    {Location.loc_start = loc_at bp lnum bolp;
     Location.loc_end = loc_at ep lnuml bolpl;
     Location.loc_ghost = bp = 0 && ep = 0}
;

value mkloc loc =
  let fname = Ploc.file_name loc in
  let bp = Ploc.first_pos loc in
  let ep = Ploc.last_pos loc in
  let lnum = Ploc.line_nb loc in
  let bolp = Ploc.bol_pos loc in
  let lnuml = Ploc.line_nb_last loc in
  let bolpl = Ploc.bol_pos_last loc in
  ocaml_location (fname, lnum, bolp, lnuml, bolpl, bp, ep)
;

value start_position_of_loc loc =
  let loc = mkloc loc in
  loc.Location.loc_start
;

value end_position_of_loc loc =
  let loc = mkloc loc in
  loc.Location.loc_end
;

value quote_position loc p =
  let open Lexing in
  <:expr< let open Lexing in {
  pos_fname = $str:p.pos_fname$ ;
  pos_lnum = $int:string_of_int p.pos_lnum$ ;
  pos_bol = $int:string_of_int p.pos_bol$ ;
  pos_cnum = $int:string_of_int p.pos_cnum$ } >>
;

value loc_of_type_decl td = fst (uv td.tdNam) ;

value option_map f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value vala_map f =
    fun
    [ Ploc.VaAnt s -> Ploc.VaAnt s
    | Ploc.VaVal x -> Ploc.VaVal (f x) ]
;

module AList = struct

value cmpequal a b = a=b ;

value assoc ?{cmp=cmpequal} x l =
  let rec arec x = fun [
    [] -> raise Not_found
  | [ (a,b)::l ] -> if cmp a x then b else arec x l
  ] in
  arec x l
;

value mem ?{cmp=cmpequal} x l = 
  let rec mrec x = fun [
    [] -> False
  | [ (a, _) :: l ] -> cmp a x || mrec x l
  ] in
  mrec x l
;

value remove ?{cmp=cmpequal} x l =
  let rec rrec x = fun [
    [] -> []
  | [ ((a, _) as pair) :: l ] ->
      if cmp a x then l else [ pair :: rrec x l ]
  ] in
  rrec x l
;
end
;

(* borrowed from ounit *)
value failwithf fmt = Fmt.kstr failwith fmt ;
value raise_failwith loc s = Ploc.raise loc (Failure s) ;
value raise_failwithf loc fmt = Fmt.kstr (raise_failwith loc) fmt ;
