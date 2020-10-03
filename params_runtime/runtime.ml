(* camlp5r *)
(* runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open MLast ;
open Pa_ppx_base ;
open Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;

type t += [
    Help of string [@rebind_to Arg.Help;][@name "Arg.Help";] 
  | Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

type lident = string ;
value equal_lident x y = x = y ;
type uident = string ;
value equal_uident x y = x = y ;
type alist 'a 'b = list ('a * 'b) [@@deriving eq;] ;
value equal_ctyp = Reloc.eq_ctyp ;
value equal_expr = Reloc.eq_expr ;
value equal_patt = Reloc.eq_patt ;
value equal_longid = Reloc.eq_longid ;

type case_branch = (patt * Ploc.vala (option expr) * expr) [@@deriving eq;] ;
type longid_lident = (option (Ploc.vala longid) * Ploc.vala string) [@@deriving eq;] ;

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
