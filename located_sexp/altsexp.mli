(**pp -syntax camlp5o -package sexplib,camlp5,pa_ppx_import,pa_ppx_deriving_plugins.std *)

[%%import: Sexp0.t
 [@with Ploc.t := Pa_ppx_base.Pp_MLast.Ploc.t]
][@@deriving show,eq]

module ErasingLoc : sig
  val equal : t -> t -> bool
end
val loc_of_sexp : t -> Ploc.t
val to_sexplib_sexp : t -> Sexplib.Type.t
val of_sexplib_sexp : Ploc.t -> Sexplib.Type.t -> t
val to_string : t -> string
val of_string : string -> t
val input_sexp : in_channel -> Sexp0.t
val load_sexp : string -> Sexp0.t
val pp_hum : Format.formatter -> Sexp0.t -> unit
