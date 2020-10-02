value filemod = "Test_deriving_params" ;
open OUnit2 ;
open Pa_ppx_base ;
open Ppxutil ;

open MLast ;
open Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;

type t += [
    Help of string [@rebind_to Arg.Help;][@name "Arg.Help";] 
  | Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;


value printer = fun x -> x ;

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

type a1 = int        [@@deriving (params, eq);] ;
type a2 = lident     [@@deriving (params, eq);] ;
type a3 = uident     [@@deriving (params, eq);] ;
type a4 = ctyp       [@@deriving (params, eq);] ;
type a5 = expr       [@@deriving (params, eq);] ;
type a6 = { a : int ; b : lident [@default "boo";] } [@@deriving (params, eq);] ;
type a7 = { a : int ; b : option lident } [@@deriving (params, eq);] ;
type a8 = longid [@@deriving (params, eq);] ;
type a9 = alist lident expr [@@deriving (params, eq);] ;
type a9' = alist longid_lident expr [@@deriving (params, eq);] ;
type a10 = list lident [@@deriving (params, eq);] ;
type a11 = (int * int * int * lident)[@@deriving (params, eq);] ;

value extract_case_branches = fun [
  <:expr< fun [ $list:l$ ] >> ->
    List.map (fun (p,wheno,e) ->
        match Patt.unapplist p with [
          (<:patt< $uid:uid$ >>, _) -> (uid, (p, wheno, e))
        | _ -> Ploc.raise (loc_of_patt p) (Failure "extract_case_branches: case-branches must start with a UIDENT")
        ]) l
]
;

value parse_expr s =  
  s |> Stream.of_string |> Grammar.Entry.parse Pcaml.expr
;
value parse_longident_lident s =  
  s |> Stream.of_string |> Grammar.Entry.parse Pcaml.longident_lident
;

value test_simple ctxt =
  let loc = Ploc.dummy in do {
  assert_equal ({foo| 1 |foo} |> parse_expr |> params_a1) 1
; assert_equal ({foo| foo |foo} |> parse_expr |> params_a2) "foo"
; assert_equal ~{cmp=Reloc.eq_ctyp} ({foo| [%typ: 'a list] |foo} |> parse_expr |> params_a4) <:ctyp< 'a list >>
; assert_equal ~{cmp=Reloc.eq_expr} ({foo| 1 |foo} |> parse_expr |> params_a5) <:expr< 1 >>
; assert_equal ({foo| {a = 1 ; b = foo} |foo} |> parse_expr |> params_a6) { a = 1 ; b = "foo" }
; assert_equal ({foo| {a = 1 } |foo} |> parse_expr |> params_a6) { a = 1 ; b = "boo" }
; assert_equal ({foo| {a = 1 } |foo} |> parse_expr |> params_a7) { a = 1 ; b = None }
; assert_equal ({foo| {a = 1 ; b = foo} |foo} |> parse_expr |> params_a7) { a = 1 ; b = Some "foo" }
; assert_equal ~{cmp=Reloc.eq_longid} ({foo| A.B.C |foo} |> parse_expr |> params_a8) <:longident< A . B . C >>
; assert_equal ~{cmp=equal_a9}
    ({foo| { a = 1 ; b = foo } |foo} |> parse_expr |> params_a9)
    [("a", <:expr< 1 >>); ("b", <:expr< foo >>)]
; assert_equal ~{cmp=equal_a9'}
    ({foo| { a = 1 ; b = foo } |foo} |> parse_expr |> params_a9')
    [(parse_longident_lident "a", <:expr< 1 >>); (parse_longident_lident "b", <:expr< foo >>)]
; assert_equal ~{cmp=equal_a9'}
    ({foo| { Foo.a = 1 ; Bar.Fuzz.b = foo } |foo} |> parse_expr |> params_a9')
    [(parse_longident_lident "Foo.a", <:expr< 1 >>); (parse_longident_lident "Bar.Fuzz.b", <:expr< foo >>)]
; assert_equal ({foo| [a ; b ; c] |foo} |> parse_expr |> params_a10) ["a"; "b"; "c"]
; assert_equal ({foo| (1,2,3,foo) |foo} |> parse_expr |> params_a11) (1,2,3,"foo")
}
;

type a12 = { srctype : ctyp ; dsttype : ctyp ;
             custom_branches_code : (alist lident case_branch) [@convert ([%typ: expr], extract_case_branches);][@default [];] }
           [@@deriving (params, eq);] ;

value test_a12 ctxt =
  let got = {foo| { srctype = [%typ: bool]; dsttype = [%typ: int] } ;
                    custom_branches_code = fun [ None -> e1 | Some x -> e2 ] } |foo}
          |> parse_expr |> params_a12 in
  ()
;

module MigrateParams = struct

module Dispatch1 = struct
type tyarg_t = {
  srctype : ctyp
; dsttype : ctyp
; dstmodule : option longid
; inherit_code : option expr
; code : option expr
; custom_branches_code : (alist lident case_branch) [@convert ([%typ: expr], extract_case_branches);][@default [];]
; custom_fields_code : (alist lident expr) [@default [];]
; skip_fields : list lident [@default [];]
; subs : list (ctyp * ctyp) [@default [];]
} [@@deriving (params, eq);] ;
type t = (alist lident tyarg_t) [@@deriving (params, eq);] ;


value test ctxt =

  let got =
    {foo| {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        } |foo}
    |> parse_expr |> params_tyarg_t in
  ()
;
end
;
module Migrate = struct

type t = {
 dispatchers : Dispatch1.t [@default [];]
} [@@deriving (params, eq);] ;


value test ctxt =
  let got =
    {foo| { dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = Ex_ast.AST1
        ; dstmod = Ex_ast.AST2
        ; types = [
            t1
          ; pt2
          ; t4'
          ]
        }
      ]
    ; dispatchers = {
        migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_t0 = {
          srctype = [%typ: t0]
        ; dsttype = [%typ: DST.t0]
        ; code = fun __dt__ s ->
            match int_of_string s with [
              n -> n
            | exception Failure _ -> migration_error "t0" ]
        }
      ; migrate_t2 = {
          srctype = [%typ: t2]
        ; dsttype = [%typ: DST.t2]
        ; custom_branches_code = fun [
              C true -> C 1
            | C false -> C 0
            | D -> migration_error "t2:D" ]
        }
      ; migrate_pt3 = {
          srctype = [%typ: 'a pt3]
        ; dsttype = [%typ: 'b DST.pt3]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; skip_fields = [ dropped_field ]
        ; custom_fields_code = {
            new_field = extra
          }
        }
      ; migrate_t4 = {
          srctype = [%typ: t4]
        ; dsttype = [%typ: DST.t4]
        }
      }
    } |foo}
    |> parse_expr |> params in
  ()
;
end
;

end
;

module Hashcons = struct
type external_funs_t = {
  preeq : expr
; prehash : expr
} [@@deriving (params, eq);]
;

value test_external_funs_t ctxt =
  let got =
    {foo|
      {
                           preeq = (fun f x y -> match (x,y) with [
                               (None, None) -> True
                             | (Some x, Some y) -> f x y
                             | _ -> False ])
                         ; prehash = (fun f x ->
                             Hashtbl.hash (Option.map f x))
                         }
    |foo}
    |> parse_expr |> params_external_funs_t in
  ()
;

type pertype_customization_t = {
  hashcons_module : option uident
; hashcons_constructor : option lident
} [@@deriving (params, eq);]
;

value test_pertype_customization_t ctxt =
  let got =
    {foo|
      {
                           hashcons_module = Term
                         ; hashcons_constructor = term
                         }
    |foo}
    |> parse_expr |> params_pertype_customization_t in
  ()
;

type t = {
  hashconsed_module_name : uident
; normal_module_name : option uident
; memo : (alist lident ctyp) [@default [];]
; external_types : (alist longid_lident external_funs_t) [@default [];]
; pertype_customization : (alist lident pertype_customization_t) [@default [];]
} [@@deriving (params, eq);]
;

value test ctxt =
  let got =
    {foo|
      { hashconsed_module_name = LAM2H
                     ; normal_module_name = LAM2
                     ; memo = {
                         memo_term = [%typ: term]
                       ; memo_int_term = [%typ: (int * term)]
                       ; memo_int = [%typ: int]
                       }
                     ; external_types = {
                         Option.t = {
                           preeq = (fun f x y -> match (x,y) with [
                               (None, None) -> True
                             | (Some x, Some y) -> f x y
                             | _ -> False ])
                         ; prehash = (fun f x ->
                             Hashtbl.hash (Option.map f x))
                         }
                       }
                     ; pertype_customization = {
                         term = {
                           hashcons_module = Term
                         ; hashcons_constructor = term
                         }
                       }
                     }
    |foo}
    |> parse_expr |> params in
  ()
;
end
;

module QAST = struct
type t = {
  data_source_module : expr
; quotation_source_module : option expr
; expr_meta_module : option expr
; patt_meta_module : option expr
; external_types : (alist ctyp expr) [@default [];]
; hashconsed : bool [@default False;]
} [@@deriving (params, eq);]
;

value test ctxt =
  let got =
    {foo|
      {
    data_source_module = Debruijn_hashcons.OK
  ; expr_meta_module = MetaE
  ; patt_meta_module = MetaP
  }
    |foo}
    |> parse_expr |> params in
  ()
;
end
;

value suite = "Test deriving(params)" >::: [
    "test_simple"           >:: test_simple
  ; "test_a12"              >:: test_a12
  ; "MigrateParams.Dispatch1.test"    >:: MigrateParams.Dispatch1.test
  ; "MigrateParams.Migrate.test"    >:: MigrateParams.Migrate.test
  ; "Hashcons.test_external_funs_t"    >:: Hashcons.test_external_funs_t
  ; "Hashcons.test_pertype_customization_t"    >:: Hashcons.test_pertype_customization_t
  ; "Hashcons.test"    >:: Hashcons.test
  ; "QAST.test"    >:: QAST.test
  ]
;

value _ = 
if not Sys.interactive.val then
  run_test_tt_main suite
else ()
;
