(* camlp5r *)
(* pp_MLast.ml,v *)

IFDEF BOOTSTRAP THEN

module Ploc = struct
include Pa_ppx_runtime.Exceptions.Ploc

type 'a vala = [%import: 'a Ploc.vala] [@@deriving show,eq]
end

type loc = [%import: MLast.loc] [@@deriving show]
type type_var = [%import: MLast.type_var] [@@deriving show]

[%%import: MLast.expr] [@@deriving show]

ELSE
let show_longid _ = "<longid>"
let pp_longid pps _ = Fmt.(pf pps "<longid>")
let show_longid_lident _ = "<longid_lident>"
let pp_longid_lident pps _ = Fmt.(pf pps "<longid_lident>")
let show_ctyp _ = "<ctyp>"
let pp_ctyp pps _ = Fmt.(pf pps "<ctyp>")
let show_expr _ = "<expr>"
let pp_expr pps _ = Fmt.(pf pps "<expr>")
let show_patt _ = "<patt>"
let pp_patt pps _ = Fmt.(pf pps "<patt>")
let show_loc _ = "<loc>"
let pp_loc pps _ = Fmt.(pf pps "<loc>")
let show_type_var _ = "<type_var>"
let pp_type_var pps _ = Fmt.(pf pps "<type_var>")
let show_longid _ = "<longid>"
let pp_longid pps _ = Fmt.(pf pps "<longid>")
let show_ctyp _ = "<ctyp>"
let pp_ctyp pps _ = Fmt.(pf pps "<ctyp>")
let show_poly_variant _ = "<poly_variant>"
let pp_poly_variant pps _ = Fmt.(pf pps "<poly_variant>")
let show_patt _ = "<patt>"
let pp_patt pps _ = Fmt.(pf pps "<patt>")
let show_expr _ = "<expr>"
let pp_expr pps _ = Fmt.(pf pps "<expr>")
let show_case_branch _ = "<case_branch>"
let pp_case_branch pps _ = Fmt.(pf pps "<case_branch>")
let show_module_type _ = "<module_type>"
let pp_module_type pps _ = Fmt.(pf pps "<module_type>")
let show_functor_parameter _ = "<functor_parameter>"
let pp_functor_parameter pps _ = Fmt.(pf pps "<functor_parameter>")
let show_sig_item _ = "<sig_item>"
let pp_sig_item pps _ = Fmt.(pf pps "<sig_item>")
let show_with_constr _ = "<with_constr>"
let pp_with_constr pps _ = Fmt.(pf pps "<with_constr>")
let show_module_expr _ = "<module_expr>"
let pp_module_expr pps _ = Fmt.(pf pps "<module_expr>")
let show_str_item _ = "<str_item>"
let pp_str_item pps _ = Fmt.(pf pps "<str_item>")
let show_type_decl _ = "<type_decl>"
let pp_type_decl pps _ = Fmt.(pf pps "<type_decl>")
let show_generic_constructor _ = "<generic_constructor>"
let pp_generic_constructor pps _ = Fmt.(pf pps "<generic_constructor>")
let show_extension_constructor _ = "<extension_constructor>"
let pp_extension_constructor pps _ = Fmt.(pf pps "<extension_constructor>")
let show_type_extension _ = "<type_extension>"
let pp_type_extension pps _ = Fmt.(pf pps "<type_extension>")
let show_class_type _ = "<class_type>"
let pp_class_type pps _ = Fmt.(pf pps "<class_type>")
let show_class_sig_item _ = "<class_sig_item>"
let pp_class_sig_item pps _ = Fmt.(pf pps "<class_sig_item>")
let show_class_expr _ = "<class_expr>"
let pp_class_expr pps _ = Fmt.(pf pps "<class_expr>")
let show_class_str_item _ = "<class_str_item>"
let pp_class_str_item pps _ = Fmt.(pf pps "<class_str_item>")
let show_longid_lident _ = "<longid_lident>"
let pp_longid_lident pps _ = Fmt.(pf pps "<longid_lident>")
let show_payload _ = "<payload>"
let pp_payload pps _ = Fmt.(pf pps "<payload>")
let show_attribute_body _ = "<attribute_body>"
let pp_attribute_body pps _ = Fmt.(pf pps "<attribute_body>")
let show_attribute _ = "<attribute>"
let pp_attribute pps _ = Fmt.(pf pps "<attribute>")
let show_attributes_no_anti _ = "<attributes_no_anti>"
let pp_attributes_no_anti pps _ = Fmt.(pf pps "<attributes_no_anti>")
let show_attributes _ = "<attributes>"
let pp_attributes pps _ = Fmt.(pf pps "<attributes>")

END

Pp_debug.Pp_MLast.ref_pp_longid := pp_longid ;;
Pp_debug.Pp_MLast.ref_pp_longid_lident := pp_longid_lident ;;
Pp_debug.Pp_MLast.ref_pp_ctyp := pp_ctyp ;;
Pp_debug.Pp_MLast.ref_pp_expr := pp_expr ;;
Pp_debug.Pp_MLast.ref_pp_patt := pp_patt ;;
Pp_debug.Pp_MLast.ref_pp_loc := pp_loc ;;
Pp_debug.Pp_MLast.ref_pp_type_var := pp_type_var ;;
Pp_debug.Pp_MLast.ref_pp_longid := pp_longid ;;
Pp_debug.Pp_MLast.ref_pp_ctyp := pp_ctyp ;;
Pp_debug.Pp_MLast.ref_pp_poly_variant := pp_poly_variant ;;
Pp_debug.Pp_MLast.ref_pp_patt := pp_patt ;;
Pp_debug.Pp_MLast.ref_pp_expr := pp_expr ;;
Pp_debug.Pp_MLast.ref_pp_case_branch := pp_case_branch ;;
Pp_debug.Pp_MLast.ref_pp_module_type := pp_module_type ;;
Pp_debug.Pp_MLast.ref_pp_functor_parameter := pp_functor_parameter ;;
Pp_debug.Pp_MLast.ref_pp_sig_item := pp_sig_item ;;
Pp_debug.Pp_MLast.ref_pp_with_constr := pp_with_constr ;;
Pp_debug.Pp_MLast.ref_pp_module_expr := pp_module_expr ;;
Pp_debug.Pp_MLast.ref_pp_str_item := pp_str_item ;;
Pp_debug.Pp_MLast.ref_pp_type_decl := pp_type_decl ;;
Pp_debug.Pp_MLast.ref_pp_generic_constructor := pp_generic_constructor ;;
Pp_debug.Pp_MLast.ref_pp_extension_constructor := pp_extension_constructor ;;
Pp_debug.Pp_MLast.ref_pp_type_extension := pp_type_extension ;;
Pp_debug.Pp_MLast.ref_pp_class_type := pp_class_type ;;
Pp_debug.Pp_MLast.ref_pp_class_sig_item := pp_class_sig_item ;;
Pp_debug.Pp_MLast.ref_pp_class_expr := pp_class_expr ;;
Pp_debug.Pp_MLast.ref_pp_class_str_item := pp_class_str_item ;;
Pp_debug.Pp_MLast.ref_pp_longid_lident := pp_longid_lident ;;
Pp_debug.Pp_MLast.ref_pp_payload := pp_payload ;;
Pp_debug.Pp_MLast.ref_pp_attribute_body := pp_attribute_body ;;
Pp_debug.Pp_MLast.ref_pp_attribute := pp_attribute ;;
Pp_debug.Pp_MLast.ref_pp_attributes_no_anti := pp_attributes_no_anti ;;
Pp_debug.Pp_MLast.ref_pp_attributes := pp_attributes ;;
