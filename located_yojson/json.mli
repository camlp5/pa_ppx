(**pp -syntax camlp5o -package camlp5,pa_ppx_import,pa_ppx_deriving_plugins.std *)

[%%import: Json0.t
 [@with Ploc.t := Pa_ppx_base.Pp_MLast.Ploc.t]
][@@deriving show,eq]

module ErasingLoc : sig
  val equal : t -> t -> bool
end

module Json : (Pa_json.PAHELPER with type t = Json0.t)
module JsonEOI : (Pa_json.PAHELPER with type t = Json0.t)
module JsonOrEOI : (Pa_json.PAHELPER with type t = Json0.t option)
module JsonList : (Pa_json.PAHELPER with type t = Json0.t list)
module JsonListEOI : (Pa_json.PAHELPER with type t = Json0.t list)

val loc_of_json : t -> Ploc.t
val to_string : t -> string
val to_yojson_json : t -> Yojson.Safe.t
val of_yojson_json : Ploc.t -> Yojson.Safe.t -> t
val pp_hum : Format.formatter -> Json0.t -> unit
