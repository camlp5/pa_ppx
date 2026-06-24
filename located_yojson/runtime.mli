(**pp -syntax camlp5o *)

type json = Json.t

module Stdlib = Stdlib
module Fmt = Fmt
val map_bind :
  ('a -> ('b, 'c) Rresult.result) ->
  'b list -> 'a list -> ('b list, 'c) Rresult.result
val safe_map : ('a -> 'b) -> 'a list -> 'b list
val result_to_located_yojson :
  ('a -> json) ->
  ('c -> json) -> ('a, 'c) result -> json
module Yojson :
  sig
    type msg = Ploc.t * string
    val result_of_located_yojson :
      (json -> ('c, msg) Rresult.result) ->
      (json -> ('d, msg) Rresult.result) ->
      json ->
      (('c, 'd) result, msg) Rresult.result
    val hashtbl_to_located_yojson :
      ('a -> json) ->
      ('c -> json) ->
      ('a, 'c) Hashtbl.t -> json
    val hashtbl_of_located_yojson :
      (json -> ('a, msg) Rresult.result) ->
      (json -> ('b, msg) Rresult.result) ->
      json -> (('a, 'b) Hashtbl.t, msg) Rresult.result
    val unit_to_located_yojson : unit -> json
    val int_to_located_yojson : int -> json
    val bool_to_located_yojson : bool -> json
    val int32_to_located_yojson : int32 -> json
    val int64_to_located_yojson : int64 -> json
    val string_to_located_yojson : string -> json
    val nativeint_to_located_yojson : nativeint -> json
    val float_to_located_yojson : float -> json
    val list_to_located_yojson : ('a -> json) -> 'a list -> json
    val array_to_located_yojson : ('a -> json) -> 'a array -> json
    val ref_to_located_yojson : ('a -> json) -> 'a ref -> json
    val option_to_located_yojson : ('a -> json) -> 'a option -> json
    val unit_of_located_yojson : string -> json -> (unit, msg) Result.result
    val int_of_located_yojson : string -> json -> (int, msg) Result.result
    val bool_of_located_yojson : string -> json -> (bool, msg) Result.result
    val int32_of_located_yojson :
      string -> json -> (int32, msg) Result.result
    val int64_of_located_yojson :
      string -> json -> (int64, msg) Result.result
    val string_of_located_yojson : string -> json -> (string, msg) Result.result
    val nativeint_of_located_yojson :
      string ->
      json -> (nativeint, msg) Result.result
    val float_of_located_yojson :
      string ->
      json ->
      (float, msg) Result.result
    val list_of_located_yojson :
      string ->
      (json -> ('c, msg) Rresult.result) ->
      json -> ('c list, msg) Rresult.result
    val array_of_located_yojson :
      string ->
      (json -> ('c, msg) Rresult.result) ->
      json -> ('c array, msg) Rresult.result
    val ref_of_located_yojson :
      (json -> ('b, msg) Rresult.result) -> json -> ('b ref, msg) Rresult.result
    val option_of_located_yojson :
      (json -> ('b, msg) result) ->
      json -> ('b option, msg) Result.result
  end
