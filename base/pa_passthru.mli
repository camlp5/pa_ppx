(**pp -syntax camlp5o *)
val debug : bool ref
val class_infos_map :
  'a ->
  attributes:('a -> MLast.attributes -> MLast.attributes) ->
  ('b -> 'c) -> 'b MLast.class_infos -> 'c MLast.class_infos
type scratchdata_t = ..
module rec EF :
  sig
    type 'a extension_point =
        ('a, Ctxt.t -> (Ctxt.t -> 'a -> 'a) -> 'a option) Extfun.t
    type t = {
      ctyp : MLast.ctyp extension_point;
      generic_constructor : MLast.generic_constructor extension_point;
      patt : MLast.patt extension_point;
      case_branch : MLast.case_branch extension_point;
      expr : MLast.expr extension_point;
      module_type : MLast.module_type extension_point;
      signature : MLast.sig_item list extension_point;
      sig_item : MLast.sig_item extension_point;
      with_constr : MLast.with_constr extension_point;
      longid : MLast.longid extension_point;
      module_expr : MLast.module_expr extension_point;
      structure : MLast.str_item list extension_point;
      str_item : MLast.str_item extension_point;
      type_decl : MLast.type_decl extension_point;
      type_extension : MLast.type_extension extension_point;
      extension_constructor : MLast.extension_constructor extension_point;
      class_type : MLast.class_type extension_point;
      class_sig_item : MLast.class_sig_item extension_point;
      class_expr : MLast.class_expr extension_point;
      class_str_item : MLast.class_str_item extension_point;
      attribute_body : MLast.attribute_body extension_point;
      implem :
        ((MLast.str_item * MLast.loc) list * Pcaml.status) extension_point;
      interf :
        ((MLast.sig_item * MLast.loc) list * Pcaml.status) extension_point;
      top_phrase : MLast.str_item option extension_point;
      use_file : (MLast.str_item list * bool) extension_point;
    }
    val mk : unit -> t
  end
and Ctxt :
  sig
    type t = {
      filename : string;
      _module_path : string list;
      options : (string * MLast.expr) list;
      ef : EF.t;
      scratch : (string * scratchdata_t) list;
      refscratch : (string * scratchdata_t) list ref;
    }
    val mk : EF.t -> Ploc.t -> t
    val append_module : t -> string -> t
    val module_path : t -> string list
    val module_path_s : t -> string
    val set_module_path : t -> string list -> t
    val filename : t -> string
    val set_filename : t -> string -> t
    val add_options : t -> (string * MLast.expr) list -> t
    val option : t -> string -> MLast.expr
    val options : t -> (string * MLast.expr) list
    val scratchdata : t -> string -> scratchdata_t
    val init_scratchdata : t -> string -> scratchdata_t -> t
    val update_scratchdata : t -> string -> scratchdata_t -> t
    val refscratchdata : t -> string -> scratchdata_t
    val init_refscratchdata : t -> string -> scratchdata_t -> unit
  end
val ctyp : Ctxt.t -> MLast.ctyp -> MLast.ctyp
val ctyp0 : Ctxt.t -> MLast.ctyp -> MLast.ctyp
val generic_constructor :
  Ctxt.t -> MLast.generic_constructor -> MLast.generic_constructor
val generic_constructor0 :
  Ctxt.t -> MLast.generic_constructor -> MLast.generic_constructor
val poly_variant : Ctxt.t -> MLast.poly_variant -> MLast.poly_variant
val patt : Ctxt.t -> MLast.patt -> MLast.patt
val patt0 : Ctxt.t -> MLast.patt -> MLast.patt
val expr : Ctxt.t -> MLast.expr -> MLast.expr
val expr0 : Ctxt.t -> MLast.expr -> MLast.expr
val case_branch : Ctxt.t -> MLast.case_branch -> MLast.case_branch
val case_branch0 : Ctxt.t -> MLast.case_branch -> MLast.case_branch
val module_type : Ctxt.t -> MLast.module_type -> MLast.module_type
val module_type0 : Ctxt.t -> MLast.module_type -> MLast.module_type
val signature : Ctxt.t -> MLast.sig_item list -> MLast.sig_item list
val signature0 : Ctxt.t -> MLast.sig_item list -> MLast.sig_item list
val sig_item : Ctxt.t -> MLast.sig_item -> MLast.sig_item
val sig_item0 : Ctxt.t -> MLast.sig_item -> MLast.sig_item
val with_constr : Ctxt.t -> MLast.with_constr -> MLast.with_constr
val with_constr0 : Ctxt.t -> MLast.with_constr -> MLast.with_constr
val longid : Ctxt.t -> MLast.longid -> MLast.longid
val longid0 : Ctxt.t -> MLast.longid -> MLast.longid
val module_expr : Ctxt.t -> MLast.module_expr -> MLast.module_expr
val module_expr0 : Ctxt.t -> MLast.module_expr -> MLast.module_expr
val structure : Ctxt.t -> MLast.str_item list -> MLast.str_item list
val structure0 : Ctxt.t -> MLast.str_item list -> MLast.str_item list
val str_item : Ctxt.t -> MLast.str_item -> MLast.str_item
val str_item0 : Ctxt.t -> MLast.str_item -> MLast.str_item
val type_decl : Ctxt.t -> MLast.type_decl -> MLast.type_decl
val type_decl0 : Ctxt.t -> MLast.type_decl -> MLast.type_decl
val type_extension : Ctxt.t -> MLast.type_extension -> MLast.type_extension
val type_extension0 : Ctxt.t -> MLast.type_extension -> MLast.type_extension
val extension_constructor :
  Ctxt.t -> MLast.extension_constructor -> MLast.extension_constructor
val extension_constructor0 :
  Ctxt.t -> MLast.extension_constructor -> MLast.extension_constructor
val class_type : Ctxt.t -> MLast.class_type -> MLast.class_type
val class_type0 : Ctxt.t -> MLast.class_type -> MLast.class_type
val class_sig_item : Ctxt.t -> MLast.class_sig_item -> MLast.class_sig_item
val class_sig_item0 : Ctxt.t -> MLast.class_sig_item -> MLast.class_sig_item
val class_expr : Ctxt.t -> MLast.class_expr -> MLast.class_expr
val class_expr0 : Ctxt.t -> MLast.class_expr -> MLast.class_expr
val class_str_item : Ctxt.t -> MLast.class_str_item -> MLast.class_str_item
val class_str_item0 : Ctxt.t -> MLast.class_str_item -> MLast.class_str_item
val longid_lident : Ctxt.t -> MLast.longid_lident -> MLast.longid_lident
val attribute : Ctxt.t -> MLast.attribute -> MLast.attribute
val attribute_body : Ctxt.t -> MLast.attribute_body -> MLast.attribute_body
val attribute_body0 : Ctxt.t -> MLast.attribute_body -> MLast.attribute_body
val attributes_no_anti :
  Ctxt.t -> MLast.attributes_no_anti -> MLast.attributes_no_anti
val attributes : Ctxt.t -> MLast.attributes -> MLast.attributes
val implem :
  Ctxt.t ->
  (MLast.str_item * MLast.loc) list * Pcaml.status ->
  (MLast.str_item * MLast.loc) list * Pcaml.status
val implem0 :
  Ctxt.t ->
  (MLast.str_item * MLast.loc) list * Pcaml.status ->
  (MLast.str_item * MLast.loc) list * Pcaml.status
val interf :
  Ctxt.t ->
  (MLast.sig_item * MLast.loc) list * Pcaml.status ->
  (MLast.sig_item * MLast.loc) list * Pcaml.status
val interf0 :
  Ctxt.t ->
  (MLast.sig_item * MLast.loc) list * Pcaml.status ->
  (MLast.sig_item * MLast.loc) list * Pcaml.status
val top_phrase : Ctxt.t -> MLast.str_item option -> MLast.str_item option
val top_phrase0 : Ctxt.t -> MLast.str_item option -> MLast.str_item option
val use_file :
  Ctxt.t -> MLast.str_item list * bool -> MLast.str_item list * bool
val use_file0 :
  Ctxt.t -> MLast.str_item list * bool -> MLast.str_item list * bool
type pass_t = {
  name : string;
  pass : int option;
  before : string list;
  after : string list;
  ef : EF.t;
}
val onepass : (Ctxt.t -> 'a -> 'b) -> Ctxt.t * 'a -> pass_t -> Ctxt.t * 'b
val tsort_passes : pass_t list -> pass_t list
val partition_passes : pass_t list -> pass_t list list
val sort_passes : pass_t list -> pass_t list
val passthru :
  ('a -> Ploc.t) -> (Ctxt.t -> 'a -> 'a) -> pass_t list -> 'a -> 'a
val eflist : pass_t list ref
val install : pass_t -> unit
val loc_of_implem : ('a * Ploc.t) list * 'b -> Ploc.t
val loc_of_interf : ('a * Ploc.t) list * 'b -> Ploc.t
val loc_of_top_phrase : MLast.str_item option -> MLast.loc
val loc_of_use_file : MLast.str_item list * 'a -> MLast.loc
