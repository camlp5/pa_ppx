value uv : Ploc.vala α → α;
value with_buffer_formatter : (Format.formatter → α → β) → α → string;
value duplicated : list string → bool;
value filter_split : (α → bool) → list α → (list α * list α);
value count : (α → bool) → list α → int;
value attr_id : MLast.attribute → string;
value module_expr_of_longident : MLast.longid → MLast.module_expr;
value longid_of_expr : MLast.expr -> MLast.longid ;
value expr_of_longid : MLast.longid -> MLast.expr ;
value convert_down_list_expr : (MLast.expr -> 'a) -> MLast.expr -> list 'a ;
value convert_up_list_expr : Ploc.t -> list MLast.expr -> MLast.expr ;
module Env : sig
  type t 'a = list (string * 'a) ;
  value add : Ploc.t -> t 'a -> string -> 'a -> t 'a ;
  value append : Ploc.t -> t 'a -> t 'a -> t 'a ;
end
;
module Expr :
  sig
    value print : MLast.expr → string;
    value to_string_list : MLast.expr → list string;
    value prepend_longident : MLast.longid → MLast.expr → MLast.expr;
    value abstract_over : list MLast.patt → MLast.expr → MLast.expr;
    value applist : MLast.expr → list MLast.expr → MLast.expr;
    value unapplist : MLast.expr → (MLast.expr * list MLast.expr);
    value tuple : MLast.loc -> list MLast.expr -> MLast.expr ;
  end
;
module Patt :
  sig
    value applist : MLast.patt → list MLast.patt → MLast.patt;
    value unapplist : MLast.patt → (MLast.patt * list MLast.patt);
    value wrap_attrs : MLast.patt → list MLast.attribute → MLast.patt;
    value unwrap_attrs : MLast.patt → (MLast.patt * list MLast.attribute);
    value tuple : MLast.loc -> list MLast.patt -> MLast.patt ;
  end
;
module Ctyp :
  sig
    value print : MLast.ctyp → string;
    value arrows_list : MLast.loc → list MLast.ctyp → MLast.ctyp → MLast.ctyp;
    value wrap_attrs : MLast.ctyp → list MLast.attribute → MLast.ctyp;
    value unwrap_attrs : MLast.ctyp → (MLast.ctyp * list MLast.attribute);
    value applist : MLast.ctyp → list MLast.ctyp → MLast.ctyp;
    value unapplist : MLast.ctyp → (MLast.ctyp * list MLast.ctyp);
    value tuple : MLast.loc -> list MLast.ctyp -> MLast.ctyp ;

    type rho = Env.t MLast.ctyp ;
    value subst : rho -> MLast.ctyp -> MLast.ctyp;
  end
;
module Longid : sig value to_string_list : MLast.longid → list string; end;
value is_poly_variant : MLast.ctyp → bool;
value is_generative_type : MLast.ctyp → bool;
value ocaml_location :
  (string * int * int * int * int * int * int) → Location.t;
value mkloc : Ploc.t → Location.t;
value start_position_of_loc : Ploc.t → Lexing.position;
value end_position_of_loc : Ploc.t → Lexing.position;
value quote_position : MLast.loc → Lexing.position → MLast.expr;
value loc_of_type_decl : MLast.type_decl → MLast.loc;
value option_map : ('a -> 'b) -> option 'a -> option 'b;
value vala_map : ('a -> 'b) -> Ploc.vala 'a -> Ploc.vala 'b;

module AList : sig
  value assoc : ?cmp:('a -> 'a -> bool) -> 'a -> list ('a * 'b) -> 'b ;
  value mem : ?cmp:('a -> 'a -> bool) -> 'a -> list ('a * 'b) -> bool ;
  value remove : ?cmp:('a -> 'a -> bool) -> 'a -> list ('a * 'b) -> list ('a * 'b) ;
end
;
value failwithf : format4 'a Format.formatter unit 'b  -> 'a ;
value raise_failwith : Ploc.t -> string -> 'a ;
value raise_failwithf : Ploc.t -> format4 'a Format.formatter unit 'b  -> 'a ;
