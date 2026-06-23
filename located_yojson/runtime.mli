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
    val result_of_located_yojson :
      (json -> ('b, string) Rresult.result) ->
      (json -> ('c, string) Rresult.result) ->
      json -> (('b, 'c) result, string) Rresult.result
    val hashtbl_to_located_yojson :
      ('a -> json) ->
      ('c -> json) ->
      ('a, 'c) Hashtbl.t -> json
    val hashtbl_of_located_yojson :
      (json -> ('a, string) Rresult.result) ->
      (json -> ('b, string) Rresult.result) ->
      json -> (('a, 'b) Hashtbl.t, string) Rresult.result
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
    val unit_of_located_yojson : string -> json -> (unit, string) Result.result
    val int_of_located_yojson : string -> json -> (int, string) Result.result
    val bool_of_located_yojson : string -> json -> (bool, string) Result.result
    val int32_of_located_yojson :
      string -> json -> (int32, string) Result.result
    val int64_of_located_yojson :
      string -> json -> (int64, string) Result.result
    val string_of_located_yojson : string -> json -> (string, string) Result.result
    val nativeint_of_located_yojson :
      string ->
      json -> (nativeint, string) Result.result
    val float_of_located_yojson :
      string ->
      json ->
      (float, string) Result.result
    val list_of_located_yojson :
      string ->
      (json -> ('c, string) Rresult.result) ->
      json -> ('c list, string) Rresult.result
    val array_of_located_yojson :
      string ->
      (json -> ('c, string) Rresult.result) ->
      json -> ('c array, string) Rresult.result
    val ref_of_located_yojson :
      (json -> ('b, 'c) Rresult.result) -> json -> ('b ref, 'c) Rresult.result
    val option_of_located_yojson :
      (json -> ('b, 'c) result) ->
      json -> ('b option, 'c) Result.result
  end
