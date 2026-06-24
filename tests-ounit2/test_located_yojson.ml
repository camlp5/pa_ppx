(**pp -syntax camlp5o -package $(PAPACKAGES),$(PACKAGEPREFIX)import -ppopt -pa_import-package -ppopt pa_ppx_located_sexp *)
open OUnit2
open Pa_ppx_testutils

module YJ = struct
type json =
        [ `Assoc of (string * json) list
        | `Bool of bool
        | `Float of float
        | `Int of int
        | `Intlit of string
        | `List of json list
        | `Null
        | `String of string ]
  [@@deriving show]
end

module LYJ = struct
open Pa_ppx_located_yojson
type json = Json.t
[@@deriving show]
end

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

let good str =
  let yojson_yojson = Yojson.Safe.from_string str in
  let located_yojson = Pa_ppx_located_yojson.Json.JsonEOI.of_string str in
  assert_equal ~printer:YJ.show_json ~msg:str (Pa_ppx_located_yojson.Json.to_yojson_json located_yojson) yojson_yojson

let bad ~yojson_msg ~msg str =
  Testutil.assert_raises_exn_pattern yojson_msg (fun _ -> Yojson.Safe.from_string str) ;
  Testutil.assert_raises_exn_pattern msg (fun _ -> Pa_ppx_located_yojson.Json.JsonEOI.of_string str) ;
  ()

let test_good ctxt =
  ()
  ; good "{}"
  ; good {|"a"|}
  ; good {|["a"]|}
  ; good {|49|}
  ; good {|2147483647|}
  ; good {|-2147483648|}
  ; good {|9223372036854775807|}
  ; good {|-9223372036854775807|}
  ; good {|1.0|}
  ; good {|-1.0|}
    (* multiple sexps *)
 
open Pa_ppx_located_yojson

let test_equality ctxt =
  let e1 = Json.JsonEOI.of_string "{}" in
  let e2 = Json.JsonEOI.of_string " {}" in
  assert_bool "changed location" (not (e1 = e2))
; assert_bool "changed location/generated default equality" (not (Json.equal e1 e2))
; assert_bool "changed location/custom equality" (Json.ErasingLoc.equal e1 e2)

let suite = "Test located_sexp" >::: [
    "test_good"   >:: test_good
    ; "test_equality"   >:: test_equality
  ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
