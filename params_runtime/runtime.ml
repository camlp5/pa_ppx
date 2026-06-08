(**pp -syntax camlp5r *)
(* camlp5r *)
(* runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open MLast ;
open Pa_ppx_base ;
open Pp_MLast ;

type lident = string ;
value equal_lident x y = x = y ;
type uident = string ;
value equal_uident x y = x = y ;
type ne_list 'a = list 'a [@@deriving eq;] ;
type alist 'a 'b = list ('a * 'b) [@@deriving eq;] ;
value equal_ctyp = Reloc.eq_ctyp ;
value equal_expr = Reloc.eq_expr ;
value equal_patt = Reloc.eq_patt ;
value equal_longid = Reloc.eq_longid ;

type case_branch = (patt * Ploc.vala (option expr) * expr) [@@deriving eq;] ;
type longid_lident = (option (Ploc.vala longid) * Ploc.vala string) [@@deriving eq;] ;

value convert_down_ne_list_expr conv1 = fun [
  <:expr< [$_$ :: $_$] >> as e -> Pa_ppx_base.Ppxutil.convert_down_list_expr conv1 e
| <:expr< ( $list:l$ ) >> when l <> [] -> List.map conv1 l
| e -> [conv1 e]
]
;

value pp_expr pps x = MLPrinters.OP.Pretty.pp_expr pps x ;
value pp_ctyp pps x = MLPrinters.OP.Pretty.pp_ctyp pps x ;
value pp_patt pps x = MLPrinters.OP.Pretty.pp_patt pps x ;
value pp_longid pps x = MLPrinters.OP.Pretty.pp_longident pps x ;
value pp_lident pps x = Fmt.(pf pps "%s" x) ;
value pp_alist ppk ppv pps l =
  Fmt.(pf pps "%a" (list (pair ppk ppv)) l) ;
value show_ctyp x = MLPrinters.OP.Pretty.show_ctyp x ;
value pp_case_branch pps b =
  let show b = MLPrinters.OP.O.match_assoc False Pprintf.empty_pc (b, False) in
  Fmt.(pf pps "%s" (show b))
;
