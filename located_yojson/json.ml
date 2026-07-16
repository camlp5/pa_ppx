(**pp -syntax camlp5o -package pa_ppx_import,pa_ppx_deriving_plugins.std *)

open Pa_ppx_base
open Ppxutil

[%%import: Json0.t
 [@with Ploc.t := Pa_ppx_base.Pp_MLast.Ploc.t]
][@@deriving show,eq]

module ErasingLoc = struct
type ploc_t = Pa_ppx_base.Pp_MLast.Ploc.t[@equal fun x y -> true][@@deriving show,eq]
[%%import: Json0.t
 [@with Ploc.t := ploc_t]
][@@deriving show,eq]
end

let loc_of_json = Json0.loc_of_json
let to_yojson_json e =
  let module Y = Yojson.Safe in
  let rec convrec j : Y.t = match j with
      (_, `Assoc l) -> `Assoc (List.map (fun (k,v) -> (k,convrec v)) l)
    | (_, `Bool b) -> `Bool b
    | (_, `Float f) -> `Float f
    | (_, `Int n) -> `Int n
    | (_, `Intlit s) -> `Intlit s
    | (_, `List l) -> `List (List.map convrec l)
    | (_, `Null) -> `Null
    | (_, `String s) -> `String s
  in convrec e
let to_yojson = to_yojson_json

let of_yojson_json loc e =
  let module Y = Yojson.Safe in
  let rec convrec t = function
      `Assoc l -> (t, `Assoc (List.map (fun (k,v) -> (k, convrec t v)) l))
    | `Bool b -> (t, `Bool b)
    | `Float f -> (t, `Float f)
    | `Int n -> (t, `Int n)
    | `Intlit s -> (t, `Intlit s)
    | `List l -> (t, `List (List.map (convrec t) l))
    | `Null -> (t, `Null)
    | `String s -> (t, `String s)
  in convrec loc e
let of_yojson = of_yojson_json Ploc.dummy

module Json = Pa_json.Json
module JsonEOI = Pa_json.JsonEOI
module JsonOrEOI = Pa_json.JsonOrEOI
module JsonList = Pa_json.JsonList
module JsonListEOI = Pa_json.JsonListEOI

let to_string se = Yojson.Safe.to_string (to_yojson_json se)
let pp_hum ?std pps se =
  Fmt.(pf pps "%a@." (Yojson.Safe.pretty_print ?std) (to_yojson_json se))

let pp_hum_to_channel ?std oc j =
  Yojson.Safe.pretty_to_channel ?std oc (to_yojson_json j) ;
  output_string oc "\n" ;
  flush oc

let raise_failwith_error_msg = function
    Result.Ok x -> x
  | Error (loc,msg) ->
     Fmt.(raise_failwith loc msg)
