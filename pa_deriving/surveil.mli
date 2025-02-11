(**pp -syntax camlp5r *)
module DerivingConfig :
  sig
    value addset : ref (list α) → α → unit;
    value addsetl : ref (list α) → list α → unit;
    type form_t = [ Short | Medium | Long ];
    type t =
      { all_plugins : ref (list string);
        all_attributes : ref (list string);
        current_plugins : ref (list string);
        current_attributes : ref (list string);
        allowed_form : ref (option (Ploc.t * form_t)) }
    ;
    value mk : unit → t;
    type Pa_ppx_base.Pa_passthru.scratchdata_t +=
      [ Pa_deriving of t ]
    ;
    value get : Pa_ppx_base.Pa_passthru.Ctxt.t → t;
    value init : Pa_ppx_base.Pa_passthru.Ctxt.t → unit;
    value legitimate_plugin_reference :
      α → (string * list (string * MLast.expr)) → bool;
    value start_decl :
      Ploc.t → t → list (string * list (string * MLast.expr)) →
        list (string * list (string * MLast.expr));
    value end_decl : t → list string;
    value set_form : Ploc.t -> t → form_t → unit;
    value get_form : t → form_t;
    value dump : Fmt.t t;
    value allowed_attribute : t → string → string → option string;
    value is_allowed_attribute :
      t → string → string → MLast.attribute → bool;
  end
;
module alias DC = DerivingConfig;
value add_current_attribute : Pa_ppx_base.Pa_passthru.Ctxt.t -> string -> unit;
value add_deriving_attributes :
  Pa_ppx_base.Pa_passthru.Ctxt.t ->
           list MLast.attribute -> list (string * list (string * MLast.expr)) ;

value sig_item :
           Pa_ppx_base.Pa_passthru.Ctxt.t ->
           (Pa_ppx_base.Pa_passthru.Ctxt.t -> MLast.sig_item -> MLast.sig_item) ->
           MLast.sig_item -> MLast.sig_item ;
value str_item :
           Pa_ppx_base.Pa_passthru.Ctxt.t ->
           (Pa_ppx_base.Pa_passthru.Ctxt.t -> MLast.str_item -> MLast.str_item) ->
           MLast.str_item -> MLast.str_item ;
value install : unit → unit;
