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
  
