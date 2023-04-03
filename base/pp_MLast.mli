(**pp -syntax camlp5r *)
(* camlp5r *)
(* pp_MLast.ml,v *)

declare
  value show_longid : MLast.longid → string;
  value show_longid_lident : MLast.longid_lident → string;
  value pp_longid_lident : Fmt.t MLast.longid_lident;
  value show_ctyp : MLast.ctyp → string;
  value pp_ctyp : Fmt.t MLast.ctyp;
  value show_expr : MLast.expr → string;
  value pp_expr : Fmt.t MLast.expr;
  value pp_attribute : Fmt.t MLast.attribute;
end;


