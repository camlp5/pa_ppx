(* camlp5r *)
(* pp_MLast.ml,v *)

IFDEF BOOTSTRAP THEN

module Ploc = struct
include Ploc


let pp0_loc ppf loc =
  let fname = Ploc.file_name loc in
  let line = Ploc.line_nb loc in
  let bp = Ploc.first_pos loc in
  let ep = Ploc.last_pos loc in
  let bol = Ploc.bol_pos loc in

  let bp = bp - bol + 1 in
  let ep = ep - bol + 1 in
  Fmt.(pf ppf "<%a:%d:%d-%d>" (quote string) fname line bp ep)

let pp1_loc ppf x = Fmt.(const string "<loc>" ppf ())

let pp_loc_verbose = ref false

let pp ppf x =
  if !pp_loc_verbose then
    pp0_loc ppf x
  else
    pp1_loc ppf x

let equal (x : t) y = x = y

type 'a vala = [%import: 'a Ploc.vala] [@@deriving show,eq]
end

type loc = [%import: MLast.loc] [@@deriving show]
type type_var = [%import: MLast.type_var] [@@deriving show]

[%%import: MLast.expr] [@@deriving show]

ELSE
let show_longid _ = "<longid>"
let show_longid_lident _ = "<longid_lident>"
let pp_longid_lident pps x = Fmt.(pf pps "<longid_lident>")
let show_ctyp _ = "<ctyp>"
let pp_ctyp pps x = Fmt.(pf pps "<ctyp>")
let show_expr _ = "<expr>"
let pp_expr pps x = Fmt.(pf pps "<expr>")
let show_patt _ = "<patt>"
let pp_patt pps x = Fmt.(pf pps "<patt>")
let pp_attribute pps x = Fmt.(pf pps "<attribute>")

END

Pp_debug.Pp_MLast.ref_show_longid := show_longid ;;
Pp_debug.Pp_MLast.ref_show_longid_lident := show_longid_lident ;;
Pp_debug.Pp_MLast.ref_show_ctyp := show_ctyp ;;
Pp_debug.Pp_MLast.ref_show_expr := show_expr ;;
Pp_debug.Pp_MLast.ref_show_patt := show_patt ;;

Pp_debug.Pp_MLast.ref_show_longid := show_longid ;;
Pp_debug.Pp_MLast.ref_show_longid_lident := show_longid_lident ;;
Pp_debug.Pp_MLast.ref_show_ctyp := show_ctyp ;;
Pp_debug.Pp_MLast.ref_show_expr := show_expr ;;
Pp_debug.Pp_MLast.ref_show_patt := show_patt ;;
Pp_debug.Pp_MLast.ref_show_loc := show_loc ;;
Pp_debug.Pp_MLast.ref_show_type_var := show_type_var ;;
Pp_debug.Pp_MLast.ref_show_longid := show_longid ;;
Pp_debug.Pp_MLast.ref_show_ctyp := show_ctyp ;;
Pp_debug.Pp_MLast.ref_show_poly_variant := show_poly_variant ;;
Pp_debug.Pp_MLast.ref_show_patt := show_patt ;;
Pp_debug.Pp_MLast.ref_show_expr := show_expr ;;
Pp_debug.Pp_MLast.ref_show_case_branch := show_case_branch ;;
Pp_debug.Pp_MLast.ref_show_module_type := show_module_type ;;
Pp_debug.Pp_MLast.ref_show_functor_parameter := show_functor_parameter ;;
Pp_debug.Pp_MLast.ref_show_sig_item := show_sig_item ;;
Pp_debug.Pp_MLast.ref_show_with_constr := show_with_constr ;;
Pp_debug.Pp_MLast.ref_show_module_expr := show_module_expr ;;
Pp_debug.Pp_MLast.ref_show_str_item := show_str_item ;;
Pp_debug.Pp_MLast.ref_show_type_decl := show_type_decl ;;
Pp_debug.Pp_MLast.ref_show_generic_constructor := show_generic_constructor ;;
Pp_debug.Pp_MLast.ref_show_extension_constructor := show_extension_constructor ;;
Pp_debug.Pp_MLast.ref_show_type_extension := show_type_extension ;;
Pp_debug.Pp_MLast.ref_show_class_type := show_class_type ;;
Pp_debug.Pp_MLast.ref_show_class_sig_item := show_class_sig_item ;;
Pp_debug.Pp_MLast.ref_show_class_expr := show_class_expr ;;
Pp_debug.Pp_MLast.ref_show_class_str_item := show_class_str_item ;;
Pp_debug.Pp_MLast.ref_show_longid_lident := show_longid_lident ;;
Pp_debug.Pp_MLast.ref_show_payload := show_payload ;;
Pp_debug.Pp_MLast.ref_show_attribute_body := show_attribute_body ;;
Pp_debug.Pp_MLast.ref_show_attribute := show_attribute ;;
Pp_debug.Pp_MLast.ref_show_attributes_no_anti := show_attributes_no_anti ;;
Pp_debug.Pp_MLast.ref_show_attributes := show_attributes ;;
