value filemod = "Test_deriving_params" ;
open OUnit2 ;
open Pa_ppx_base ;
open Ppxutil ;

open MLast ;
open Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;
open Pa_ppx_params.Runtime ;
open Pa_ppx_testutils ;

type a0 = bool        [@@deriving (params { validator = fun x -> Result.Ok x }, eq);] ;
type a0' = bool        [@@deriving (params { validator = fun x -> if x then Result.Ok x else Result.Error "message" }, eq);] ;
type a1 = int        [@@deriving (params, eq);] ;
type a2 = lident     [@@deriving (params, eq);] ;
type a3 = uident     [@@deriving (params, eq);] ;
type a4 = ctyp       [@@deriving (params, eq);] ;
type a5 = expr       [@@deriving (params, eq);] ;
type a6 = { a : int ; b : lident [@default "boo";] } [@@deriving (params, eq);] ;
type a6' = { a' : int ; b' : lident [@default "boo";][@name c;] } [@@deriving (params, eq);] ;
type a7 = { a : int ; b : option lident } [@@deriving (params, eq);] ;
type a8 = longid [@@deriving (params, eq);] ;
type a9 = alist lident expr [@@deriving (params, eq);] ;
type a9' = alist longid_lident expr [@@deriving (params, eq);] ;
type a10 = list lident [@@deriving (params, eq);] ;
type a11 = (int * int * int * lident)[@@deriving (params, eq);] ;

value extract_case_branches = fun [
  None -> []
| Some <:expr< fun [ $list:l$ ] >> ->
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
  assert_equal ({foo| True |foo} |> parse_expr |> params_a0) True
; assert_equal ({foo| 1 |foo} |> parse_expr |> params_a1) 1
; assert_equal ({foo| foo |foo} |> parse_expr |> params_a2) "foo"
; assert_equal ~{cmp=Reloc.eq_ctyp} ({foo| [%typ: 'a list] |foo} |> parse_expr |> params_a4) <:ctyp< 'a list >>
; assert_equal ~{cmp=Reloc.eq_expr} ({foo| 1 |foo} |> parse_expr |> params_a5) <:expr< 1 >>
; assert_equal ({foo| {a = 1 ; b = foo} |foo} |> parse_expr |> params_a6) { a = 1 ; b = "foo" }
; assert_equal ({foo| {a' = 1 ; c = foo} |foo} |> parse_expr |> params_a6') { a' = 1 ; b' = "foo" }
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

value matches ~{pattern} text =
  match Str.search_forward (Str.regexp pattern) text 0 with [
    _ -> True
  | exception Not_found -> False
  ]
;

value assert_raises_exn_pattern pattern f =
  Testutil.assert_raises_exn_pred
    (fun [
           Ploc.Exc _ (Failure msg) when matches ~{pattern} msg -> True
         | _ -> False
         ])
      f
;

value test_errors ctxt =
  let loc = Ploc.dummy in do {
    ()
  ; assert_raises_exn_pattern "params failed validation check"
      (fun () ->
         ignore ({foo| False |foo} |> parse_expr |> params_a0))
  ; assert_raises_exn_pattern "message"
      (fun () ->
         ignore ({foo| False |foo} |> parse_expr |> params_a0'))
  ; assert_raises_exn_pattern "superfluous (not-allowed) fields: c"
      (fun () ->
         ignore ({foo| {a = 1; c = 3 } |foo} |> parse_expr |> params_a6))
  ; assert_raises_exn_pattern "field a is not optional"
      (fun () ->
         ignore ({foo| {b = foo } |foo} |> parse_expr |> params_a6))
  ; assert_raises_exn_pattern "param should be integer"
      (fun () ->
         ignore ({foo| Foo |foo} |> parse_expr |> params_a1))
  ; assert_raises_exn_pattern "param should be LIDENT"
      (fun () ->
         ignore ({foo| Foo |foo} |> parse_expr |> params_a2))
  ; assert_raises_exn_pattern "param should be UIDENT"
      (fun () ->
         ignore ({foo| foo |foo} |> parse_expr |> params_a3))
  ; assert_raises_exn_pattern "key should be of the form LIDENT"
      (fun () ->
         ignore ({foo| { a = 1 ; A.b = foo } |foo} |> parse_expr |> params_a9))
  ; assert_raises_exn_pattern "param must be a tuple of type"
      (fun () ->
         ignore ({foo| (1,2,3,foo,True) |foo} |> parse_expr |> params_a11))
  }
;

type a12 = { srctype : ctyp ; dsttype : ctyp
           ; custom_branches_code : option expr 
           ; custom_branches : (alist lident case_branch) [@computed extract_case_branches custom_branches_code;]
           } [@@deriving (params, eq);] ;

value test_a12 ctxt =
  let got = {foo| { srctype = [%typ: bool]; dsttype = [%typ: int] } ;
                    custom_branches_code = fun [ None -> e1 | Some x -> e2 ] } |foo}
          |> parse_expr |> params_a12 in
  ()
;

type a13 = { a : int
           ; b : string [@computed b;]
           }
[@@deriving (params { formal_args = { a13 = [ b ] } },
             eq);] ;

value test_a13 ctxt =
  assert_equal ({foo| { a = 1  } |foo} |> parse_expr |> params_a13 "boo") { a = 1 ; b = "boo" }
;

type a14 = { a : int
           ; c : (aux [@actual_args [b];])
           }
and aux = { e : bool ; d : string [@computed b';] }
[@@deriving (params { formal_args = {
    a14 = [ b ]
  ; aux = [ b' ]
  } },
             eq);] ;

value test_a14 ctxt =
  assert_equal
    ({foo| { a = 1 ; c = { e  = True } } |foo} |> parse_expr |> params_a14 "boo")
    { a = 1 ; c =  { e = True ; d = "boo" } }
;

module MigrateParams = struct

module Dispatch1 = struct
type tyarg_t = {
  srctype : ctyp
; dsttype : ctyp
; dstmodule : option longid
; inherit_code : option expr
; code : option expr
; custom_branches_code : option expr 
; custom_branches : (alist lident case_branch) [@computed extract_case_branches custom_branches_code;]
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

type default_dispatcher_t = {
  srcmod : longid
; dstmod : longid
; types : list lident
; inherit_code : (alist lident expr) [@default [];]
} [@@deriving (params, eq);]
;

type t = {
  inherit_type : option ctyp
; dispatch_type : lident
; dispatch_table_constructor : lident
; dispatchers : Dispatch1.t [@default [];]
; default_dispatchers : list default_dispatcher_t [@default [];]
} [@@deriving (params, eq);]
;

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
  let got =
    {foo| 
      { inherit_type = [%typ: location_t option]
    ; dispatch_type = dispatch_table_t
    ; dispatch_table_constructor = make_dt
    ; default_dispatchers = [
        {
          srcmod = All_ast.Ast_4_02
        ; dstmod = DST
        ; types = [
            lexing_position
          ; location_t
          ; location_loc
          ; longident_t
          ]
        }
      ; {
        srcmod = All_ast.Ast_4_02.Asttypes
      ; dstmod = DST.Asttypes
      ; types = [
          closed_flag
        ; direction_flag
        ; label
        ; mutable_flag
        ; override_flag
        ; private_flag
        ; rec_flag
        ; variance
        ; virtual_flag
        ]
      }
      ; {
        srcmod = All_ast.Ast_4_02.Parsetree
      ; dstmod = DST.Parsetree
      ; types = [
          attribute
        ; attributes
        ; case
        ; class_declaration
        ; class_description
        ; class_expr
        ; class_field
        ; class_field_kind
        ; class_infos
        ; class_signature
        ; class_structure
        ; class_type
        ; class_type_declaration
        ; class_type_field
        ; core_type
        ; expression
        ; extension
        ; extension_constructor
        ; include_declaration
        ; include_description
        ; include_infos
        ; label_declaration
        ; location_stack
        ; module_binding
        ; module_declaration
        ; module_expr
        ; module_expr_desc
        ; module_type
        ; module_type_declaration
        ; module_type_desc
        ; open_description
        ; package_type
        ; pattern
        ; pattern_desc
        ; payload
        ; signature
        ; signature_item
        ; structure
        ; structure_item
        ; type_declaration
        ; type_extension
        ; type_kind
        ; value_binding
        ; value_description
        ]
      ; inherit_code = {
          class_expr = Some pcl_loc
        ; class_field = Some pcf_loc
        ; class_infos = Some pci_loc
        ; class_type_field = Some pctf_loc
        ; class_type = Some pcty_loc
        ; core_type = Some ptyp_loc
        ; expression = Some pexp_loc
        ; extension_constructor = Some pext_loc
        ; include_infos = Some pincl_loc
        ; label_declaration = Some pld_loc
        ; module_binding = Some pmb_loc
        ; module_declaration = Some pmd_loc
        ; module_expr = Some pmod_loc
        ; module_type_declaration = Some pmtd_loc
        ; module_type = Some pmty_loc
        ; open_description = Some popen_loc
        ; pattern = Some ppat_loc
        ; signature_item = Some psig_loc
        ; structure_item = Some pstr_loc
        ; type_declaration = Some ptype_loc
        ; value_binding = Some pvb_loc
        ; value_description = Some pval_loc
        }
      }
      ; {
        srcmod = All_ast.Ast_4_02.Outcometree
      ; dstmod = DST.Outcometree
      ; types = [
          out_class_sig_item
        ; out_class_type
        ; out_extension_constructor
        ; out_ext_status
        ; out_ident
        ; out_module_type
        ; out_phrase
        ; out_rec_status
        ; out_type
        ; out_type_extension
        ]
      }
      ]
    ; dispatchers = {
        migrate_option = {
          srctype = [%typ: 'a option]
        ; dsttype = [%typ: 'b option]
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        ; code = (fun subrw __dt__ __inh__ x -> Option.map (subrw __dt__ __inh__) x)
        }
      ; migrate_constant = {
          srctype = [%typ: constant]
        ; dsttype = [%typ: DST.Parsetree.constant]
        ; code = migrate_Asttypes_constant_Parsetree_constant
        }
      ; migrate_list = {
          srctype = [%typ: 'a list]
        ; dsttype = [%typ: 'b list]
        ; code = _migrate_list
        ; subs = [ ([%typ: 'a], [%typ: 'b]) ]
        }
      ; migrate_core_type_desc = {
          srctype = [%typ: core_type_desc]
        ; dsttype = [%typ: DST.Parsetree.core_type_desc]
        ; custom_branches_code = fun [
            Ptyp_arrow (v_0, v_1, v_2) ->
            let open DST.Parsetree in
            Ptyp_arrow
              (migrate_label_arg_label __dt__ __inh__ v_0,
               __dt__.migrate_core_type __dt__ __inh__ v_1,
               __dt__.migrate_core_type __dt__ __inh__ v_2)
            | Ptyp_object (v_0, v_1) ->
              let open DST.Parsetree in
              Ptyp_object
                (List.map (fun (v_0, v_1, v_2) ->
                     Otag(__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                          __dt__.migrate_attributes __dt__ __inh__ v_1,
                          __dt__.migrate_core_type __dt__ __inh__ v_2)) v_0,
                 __dt__.migrate_closed_flag __dt__ __inh__ v_1)
                
            | Ptyp_poly (v_0, v_1) ->
              let open DST.Parsetree in
              Ptyp_poly
                (List.map (fun v_0 ->
                  __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0)) v_0,
                 __dt__.migrate_core_type __dt__ __inh__ v_1)
          ]
        }
      ; migrate_row_field = {
          srctype = [%typ: row_field]
        ; dsttype = [%typ: DST.Parsetree.row_field]
        ; custom_branches_code = fun [
              Rtag (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Rtag
                (__dt__.migrate_location_loc __dt__.migrate_label __dt__ __inh__ (wrap_loc __inh__ v_0),
                 __dt__.migrate_attributes __dt__ __inh__ v_1,
                 v_2,
                 List.map (__dt__.migrate_core_type __dt__ __inh__) v_3)
          ]
        }
      ; migrate_expression_desc = {
          srctype = [%typ: expression_desc]
        ; dsttype = [%typ: DST.Parsetree.expression_desc]
        ; custom_branches_code = fun [
              Pexp_fun (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Pexp_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 __dt__.migrate_option __dt__.migrate_expression __dt__ __inh__ v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_expression __dt__ __inh__ v_3)
            | Pexp_apply (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_apply
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1) v_1)
            | Pexp_send (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_send
                (__dt__.migrate_expression __dt__ __inh__ v_0,
                 __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_1))
            | Pexp_newtype (v_0, v_1) ->
              let open DST.Parsetree in
              Pexp_newtype
                (__dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                 __dt__.migrate_expression __dt__ __inh__ v_1)
          ]
        }
      ; migrate_constructor_declaration = {
          srctype = [%typ: constructor_declaration]
        ; dsttype = [%typ: DST.Parsetree.constructor_declaration]
        ; inherit_code = Some pcd_loc
        ; skip_fields = [ pcd_args ]
        ; custom_fields_code = {
            pcd_args =
              DST.Parsetree.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) pcd_args)
          }
        }
      ; migrate_extension_constructor_kind = {
          srctype = [%typ: extension_constructor_kind]
        ; dsttype = [%typ: DST.Parsetree.extension_constructor_kind]
        ; custom_branches_code = fun [
          Pext_decl (v_0, v_1) ->
          let open DST.Parsetree in
          Pext_decl
          (DST.Parsetree.Pcstr_tuple (List.map (__dt__.migrate_core_type __dt__ __inh__) v_0),
          Option.map (__dt__.migrate_core_type __dt__ __inh__) v_1)
          ]
          }
      ; migrate_class_type_desc = {
          srctype = [%typ: class_type_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_desc]
        ; custom_branches_code = fun [
          Pcty_arrow (v_0, v_1, v_2) ->
          let open DST.Parsetree in
          Pcty_arrow
          (migrate_label_arg_label __dt__ __inh__ v_0,
          __dt__.migrate_core_type __dt__ __inh__ v_1,
          __dt__.migrate_class_type __dt__ __inh__ v_2)
          ]
          }
      ; migrate_class_type_field_desc = {
          srctype = [%typ: class_type_field_desc]
        ; dsttype = [%typ: DST.Parsetree.class_type_field_desc]
        ; custom_branches_code = fun [
             Pctf_val v_0 ->
              let open DST.Parsetree in
              Pctf_val
                ((fun (v_0, v_1, v_2, v_3) ->
                    __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                    __dt__.migrate_mutable_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3)
                   v_0)

            | Pctf_method v_0 ->
              let open DST.Parsetree in
              Pctf_method
                ((fun (v_0, v_1, v_2, v_3) ->
                    __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v_0),
                    __dt__.migrate_private_flag __dt__ __inh__ v_1,
                    __dt__.migrate_virtual_flag __dt__ __inh__ v_2,
                    __dt__.migrate_core_type __dt__ __inh__ v_3) v_0)
          ]
        }
      ; migrate_class_expr_desc = {
          srctype = [%typ: class_expr_desc]
        ; dsttype = [%typ: DST.Parsetree.class_expr_desc]
        ; custom_branches_code = fun [
              Pcl_fun (v_0, v_1, v_2, v_3) ->
              let open DST.Parsetree in
              Pcl_fun
                (migrate_label_arg_label __dt__ __inh__ v_0,
                 Option.map (__dt__.migrate_expression __dt__ __inh__)  v_1,
                 __dt__.migrate_pattern __dt__ __inh__ v_2,
                 __dt__.migrate_class_expr __dt__ __inh__ v_3)
            | Pcl_apply (v_0, v_1) ->
              let open DST.Parsetree in
              Pcl_apply
                (__dt__.migrate_class_expr __dt__ __inh__ v_0,
                 List.map (fun (v_0, v_1) ->
                     migrate_label_arg_label __dt__ __inh__ v_0,
                     __dt__.migrate_expression __dt__ __inh__ v_1)
                   v_1)
          ]
        }
      ; migrate_class_field_desc = {
          srctype = [%typ: class_field_desc]
        ; dsttype = [%typ: DST.Parsetree.class_field_desc]
        ; custom_branches_code = fun [
              Pcf_inherit (v_0, v_1, v_2) ->
              let open DST.Parsetree in
              Pcf_inherit
                (__dt__.migrate_override_flag __dt__ __inh__ v_0,
                 __dt__.migrate_class_expr __dt__ __inh__ v_1,
                 Option.map (fun v -> __dt__.migrate_location_loc (fun _ _ x -> x) __dt__ __inh__ (wrap_loc __inh__ v)) v_2)
          ]
        }
      ; migrate_signature_item_desc = {
          srctype = [%typ: signature_item_desc]
        ; dsttype = [%typ: DST.Parsetree.signature_item_desc]
        ; custom_branches_code = fun [
              Psig_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Asttypes.Nonrecursive
                else DST.Asttypes.Recursive in
              let open DST.Parsetree in
              Psig_type
                (rf, List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0) ]
        }
      ; migrate_with_constraint = {
          srctype = [%typ: with_constraint]
        ; dsttype = [%typ: DST.Parsetree.with_constraint]
        ; custom_branches_code = fun [
             Pwith_typesubst x0 ->
              let lid_loc = map_loc (fun x -> Lident x) x0.ptype_name in 
              let open DST.Parsetree in
              Pwith_typesubst
                (__dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ lid_loc,
                 __dt__.migrate_type_declaration __dt__ __inh__ x0)

            | Pwith_modsubst (v_0, v_1) ->
              let lid_loc = map_loc (fun x -> Lident x) v_0 in 
              let open DST.Parsetree in
              Pwith_modsubst
                (__dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ lid_loc,
                 __dt__.migrate_location_loc __dt__.migrate_longident_t __dt__ __inh__ v_1)
          ]
        }
      ; migrate_structure_item_desc = {
          srctype = [%typ: structure_item_desc]
        ; dsttype = [%typ: DST.Parsetree.structure_item_desc]
        ; custom_branches_code = fun [
              Pstr_type v_0 ->
              let is_nonrec (attr,_) = attr.txt = "nonrec" in
              let rf = if (List.exists (fun td ->
                  List.exists is_nonrec td.ptype_attributes) v_0) then
                  DST.Asttypes.Nonrecursive
                else DST.Asttypes.Recursive in
              let open DST.Parsetree in
              Pstr_type
                (rf, List.map (__dt__.migrate_type_declaration __dt__ __inh__) v_0) ]
        }
      ; migrate_printer = {
          srctype = [%typ: (Format.formatter -> unit)]
        ; dsttype = [%typ: (Format.formatter -> unit)]
        ; code = fun _ _ x -> x
        }
      ; migrate_exn = {
          srctype = [%typ: exn]
        ; dsttype = [%typ: exn]
        ; code = fun _ _ x -> x
        }
      ; migrate_out_value = {
          srctype = [%typ: out_value]
        ; dsttype = [%typ: DST.Outcometree.out_value]
        ; custom_branches_code = fun [
             Oval_string v_0 ->
              let open DST.Outcometree in
              Oval_string (v_0, max_int, Ostr_string) ]
        }
      ; migrate_out_variant = {
          srctype = [%typ: out_variant]
        ; dsttype = [%typ: DST.Outcometree.out_variant]
        ; custom_branches_code = fun [
             Ovar_name (v_0, v_1) ->
              let open DST.Outcometree in
              Ovar_typ
                (Otyp_constr
                   (__dt__.migrate_out_ident __dt__ __inh__ v_0,
                    List.map (__dt__.migrate_out_type __dt__ __inh__) v_1)) ]
        }
      ; migrate_out_sig_item = {
          srctype = [%typ: out_sig_item]
        ; dsttype = [%typ: DST.Outcometree.out_sig_item]
        ; custom_branches_code = fun [
              Osig_value (v_0, v_1, v_2) ->
              let open DST.Outcometree in
              Osig_value
                {oval_name = v_0
                ; oval_type = __dt__.migrate_out_type __dt__ __inh__ v_1
                ; oval_prims = v_2
                ; oval_attributes = []} ]
        }
      ; migrate_out_type_decl = {
          srctype = [%typ: out_type_decl]
        ; dsttype = [%typ: DST.Outcometree.out_type_decl]
        ; custom_fields_code = {
            otype_immediate = false
          ; otype_unboxed = false
          }
        }
      }
    }
    |foo}
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
  ; "test_errors"           >:: test_errors
  ; "test_a12"              >:: test_a12
  ; "test_a13"              >:: test_a13
  ; "test_a14"              >:: test_a14
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
