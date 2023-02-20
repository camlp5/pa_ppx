(** -syntax camlp5r *)
(* pa_unmatched_vala.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value is_unmatched_vala_branches l =
  let rec isrec = fun [
    [ (<:patt< [%unmatched_vala] >>, <:vala< None >>, _) :: _ ] -> True
  | [ _ :: t ] -> isrec t
  | [] -> False
  ] in
  isrec (List.rev l)
;

value sep_unmatched_vala_branch l =
  let rec seprec acc = fun [
    [] -> assert False
  | [ ((<:patt< [%unmatched_vala] >>, <:vala< None >>, _) as b) :: t ] -> (List.rev acc, b, t)
  | [ h :: t ] -> seprec [ h::acc ] t
  ] in
  seprec [] l
;

value is_orpat = fun [ <:patt< $_$ | $_$ >> -> True | _ -> False ] ;

value unpack_orpat p =
  let rec orec = fun [
    <:patt:< $a$ | $b$ >> -> (orec a) @ (orec b)
  | p -> [p]
  ] in
  orec p
;

value strip_vars p =
  let rec strec = fun [
    <:patt:< $lid:_$ >> -> <:patt< _ >>
  | <:patt:< ($p$ as $lid:_$) >> -> strec p
  | <:patt:< $a$ $b$ >> -> <:patt:< $strec a$ $strec b$ >>
  | <:patt:< $a$ | $b$ >> -> <:patt:< $strec a$ | $strec b$ >>
  | <:patt:< ( $list:l$ ) >> -> <:patt:< ( $list:List.map strec l$ ) >>
  | <:patt:< { $list:l$ } >> ->
     let strboth (l,v) = (strec l, strec v) in
     <:patt:< { $list:List.map strboth l$ } >>
  | p -> p
  ] in
  strec p
;

value or_pattern_of_list loc pl =
  List.fold_left (fun p q -> <:patt< $p$ | $q$ >>) (List.hd pl) (List.tl pl)
;

value new_vala_covering_pattern p =
  let rec genrec p = match p with [
      <:patt:< Ploc.VaVal $_$ >> -> or_pattern_of_list loc [p ; <:patt:< Ploc.VaAnt _ >>]
    | <:patt:< $a$ $b$ >> ->
       let a = genrec a in
       let b = genrec b in
       <:patt:< $a$ $b$ >>
    | <:patt:< { $list:l$ } >> ->
       let l = List.map (fun (l,v) -> (l, genrec v)) l in
       <:patt:< { $list:l$ } >>
    | p -> p
    ]
  in
  genrec p
;

value rewrite_unmatched_vala_branches branches = do {
  assert (is_unmatched_vala_branches branches) ;
  let (prefix, ((uvpat, _, uvb_exp) as uvb), suffix) = sep_unmatched_vala_branch branches in
  let loc = loc_of_patt uvpat in
  let branchpats = List.map (fun (p, _, _) -> p) prefix in
  let branchpats = List.concat_map unpack_orpat branchpats in
  let branchpats = List.map strip_vars branchpats in
  let newpat = or_pattern_of_list loc (List.map new_vala_covering_pattern branchpats) in
  let newbranch = (newpat, <:vala< None >>, uvb_exp) in
    prefix @ [newbranch] @ suffix
}
;

value rewrite_expr arg = fun [
  <:expr:< fun [ $list:l$ ] >> ->
  let l = rewrite_unmatched_vala_branches l in
  <:expr< fun [ $list:l$ ] [@warning "-12";] >>
| <:expr:< match $e$ with [ $list:l$ ] >> ->
  let l = rewrite_unmatched_vala_branches l in
  <:expr< match $e$ with [ $list:l$ ] [@warning "-12";] >>
| _ -> assert False
]
;

value install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< match $e$ with [ $list:branches$ ] >> as z
    when is_unmatched_vala_branches branches ->
    fun arg fallback ->
      Some (rewrite_expr arg z)
  | <:expr:< fun [ $list:branches$ ] >> as z
    when is_unmatched_vala_branches branches ->
    fun arg fallback ->
      Some (fallback arg (rewrite_expr arg z))
  ] } in
  Pa_passthru.(install { name = "pa_unmatched_vala" ; ef = ef ; pass = None ; before = [] ; after = [] })
;

install();
