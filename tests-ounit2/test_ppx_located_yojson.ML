#ifdef PAPPX
(**pp -syntax camlp5o -package $(PAPACKAGES) *)
let filemod = "Test_ppx_located_yojson."
#else
(**pp -package $(PPXPACKAGES) *)
let filemod = ""
#endif
open OUnit2
#ifndef PAPPX
open Sexplib.Sexp
open Sexplib.Std
open Sexplib0.Sexp_conv
#endif

open Pa_ppx_located_yojson
type json = Json.t
[@@deriving show]

let show_error_or =
  let module M = struct
    type 'a error_or = ('a, string) Result.result [@@deriving show]
  end in
  M.show_error_or

let equal_error_or =
  let module M = struct
    type 'a error_or = ('a, string) Result.result [@@deriving eq]
  end in
  M.equal_error_or

let cmp = Json.ErasingLoc.equal

let assert_roundtrip pp_obj to_json of_json obj str =
  let json = Json.of_string str in
  let cleanup json = Json.(json |> to_string |> of_string) in
  assert_equal ~printer:Json.show json (cleanup (to_json obj));
  assert_equal ~printer:(show_error_or pp_obj) (Result.Ok obj) (of_json json)

let assert_roundtrip' pp_obj (to_json, cmp_json) (of_json, cmp_obj) obj str =
  let json = Json.of_string str in
  let cleanup json = Json.(json |> to_string |> of_string) in
  assert_equal ~cmp:cmp_json ~printer:Json.show json (cleanup (to_json obj));
  assert_equal ~cmp:cmp_obj ~printer:(show_error_or pp_obj) (Result.Ok obj) (of_json json)

let assert_roundtrip_sexp pp_obj to_sexp of_sexp obj str =
  let sexp = Sexplib.Sexp.of_string str in
  assert_equal ~printer:Sexplib0.Sexp.to_string_hum sexp (to_sexp obj);
  assert_equal ~printer:pp_obj obj (of_sexp sexp)

let assert_failure pp_obj of_json err str =
  let json = Json.of_string str in
  assert_equal ~printer:(show_error_or pp_obj) (Result.Error err) (of_json json)

type u = unit         [@@deriving show, located_yojson, sexp]
type i1 = int         [@@deriving show, located_yojson, sexp, eq]
type i2 = int32       [@@deriving show, located_yojson, sexp]
type i3 = Int32.t     [@@deriving show, located_yojson]
type i4 = int64       [@@deriving show, located_yojson, sexp]
type i5 = Int64.t     [@@deriving show, located_yojson]
type i6 = nativeint   [@@deriving show, located_yojson, sexp]
type i7 = Nativeint.t [@@deriving show, located_yojson]
type i8 = int64       [@located_yojson.encoding `string] [@@deriving show, located_yojson]
type i9 = nativeint   [@located_yojson.encoding `string] [@@deriving show, located_yojson]
type f  = float       [@@deriving show, located_yojson, sexp]
type b  = bool        [@@deriving show, located_yojson, sexp]
type c  = char        [@@deriving show, located_yojson, sexp]
type s  = string      [@@deriving show, located_yojson, sexp]
type y  = bytes       [@@deriving show, located_yojson, sexp]
type xr = int ref     [@@deriving show, located_yojson, sexp]
type xo = int option  [@@deriving show, located_yojson, sexp]
type xl = int list    [@@deriving show, located_yojson, sexp]
type xa = int array   [@@deriving show, located_yojson, sexp]
type xt = int * int   [@@deriving show, located_yojson, sexp]
type 'a p = 'a option
[@@deriving show, located_yojson, sexp]
type pv = [ `A | `B of int | `C of int * string ]
[@@deriving show, located_yojson, sexp]
type pva = [ `A ] and pvb = [ `B ]
[@@deriving show, located_yojson, sexp]
type 'a pvc = [ `C of 'a ]
[@@deriving show, located_yojson, sexp]
type pvd = [ pva | `A1 | `B1 of int | `C1 of int * string | pvb | int pvc ]
[@@deriving show, located_yojson, sexp]
type v  = A | B of int | C of int * string
[@@deriving show, located_yojson, sexp]
type r  = { x : int; y : string }
[@@deriving show, located_yojson, sexp]

type rv = RA | RB of int | RC of int * string | RD of { z : string }
[@@deriving show, located_yojson, sexp]

let test_unit _ctxt =
  assert_roundtrip' pp_u (u_to_located_yojson, cmp) (u_of_located_yojson, (=))
    () "null" ;
  assert_roundtrip_sexp show_u sexp_of_u u_of_sexp
    () "()"

let test_int _ctxt =
  assert_roundtrip_sexp show_i1 sexp_of_i1 i1_of_sexp
    1 "1" ;
  assert_roundtrip pp_i1 i1_to_located_yojson i1_of_located_yojson
                   42 "42";
  assert_roundtrip pp_i2 i2_to_located_yojson i2_of_located_yojson
                   43l "43";
  assert_roundtrip pp_i3 i3_to_located_yojson i3_of_located_yojson
                   44l "44";
  assert_roundtrip pp_i4 i4_to_located_yojson i4_of_located_yojson
                   45L "45";
  assert_roundtrip pp_i5 i5_to_located_yojson i5_of_located_yojson
                   46L "46";
  assert_roundtrip pp_i6 i6_to_located_yojson i6_of_located_yojson
                   47n "47";
  assert_roundtrip pp_i7 i7_to_located_yojson i7_of_located_yojson
                   48n "48";
  assert_roundtrip pp_i8 i8_to_located_yojson i8_of_located_yojson
                   49L "\"49\"";
  assert_roundtrip pp_i9 i9_to_located_yojson i9_of_located_yojson
                   50n "\"50\""

let test_int_edge _ctxt =
  assert_roundtrip pp_i2 i2_to_located_yojson i2_of_located_yojson
                   0x7fffffffl "2147483647";
  assert_roundtrip pp_i2 i2_to_located_yojson i2_of_located_yojson
                   (Int32.neg 0x80000000l) "-2147483648";
  assert_roundtrip pp_i4 i4_to_located_yojson i4_of_located_yojson
                   0x7fffffffffffffffL "9223372036854775807";
  assert_roundtrip pp_i4 i4_to_located_yojson i4_of_located_yojson
                   (Int64.neg 0x8000000000000000L) "-9223372036854775808"

let test_float _ctxt =
  assert_roundtrip pp_f f_to_located_yojson f_of_located_yojson
                   1.0 "1.0";
  assert_equal ~printer:(show_error_or pp_f)
               (Result.Ok 1.0)
               (f_of_located_yojson (Ploc.dummy, `Int 1))

let test_bool _ctxt =
  assert_roundtrip pp_b b_to_located_yojson b_of_located_yojson
                   true "true";
  assert_roundtrip pp_b b_to_located_yojson b_of_located_yojson
                   false "false"

let test_char _ctxt =
  assert_roundtrip pp_c c_to_located_yojson c_of_located_yojson
                   'c' "\"c\"";
  assert_failure   pp_c c_of_located_yojson
                   (filemod^"c") "\"xxx\""

let test_string _ctxt =
  assert_roundtrip pp_s s_to_located_yojson s_of_located_yojson
                   "foo" "\"foo\"";
  assert_roundtrip pp_y y_to_located_yojson y_of_located_yojson
                   (Bytes.of_string "foo") "\"foo\""

let test_ref _ctxt =
  assert_roundtrip pp_xr xr_to_located_yojson xr_of_located_yojson
                   (ref 42) "42"

let test_option _ctxt =
  assert_roundtrip pp_xo xo_to_located_yojson xo_of_located_yojson
                   (Some 42) "42";
  assert_roundtrip pp_xo xo_to_located_yojson xo_of_located_yojson
                   None "null"

let test_poly _ctxt =
  assert_roundtrip pp_xo
    (([%to_located_yojson: 'a option] [%to_located_yojson: int]))
    (([%of_located_yojson: 'a option] [%of_located_yojson: int]))
                   (Some 42) "42"

let test_list _ctxt =
  assert_roundtrip pp_xl xl_to_located_yojson xl_of_located_yojson
                   [] "[]";
  assert_roundtrip pp_xl xl_to_located_yojson xl_of_located_yojson
                   [42; 43] "[42, 43]";
  let rec make_list i acc =
            if i = 0
            then (i mod 100 :: acc)
            else make_list (i - 1) (i mod 100 :: acc) in
  let lst =  make_list (500_000 - 1) [] in
  let buf = Buffer.create (5_000 * 390 + 4) in
  Buffer.add_string buf "[";
  Buffer.add_string buf (string_of_int (List.hd lst));
  List.iter (fun x -> Buffer.add_string buf ", "; Buffer.add_string buf (string_of_int x)) (List.tl lst);
  Buffer.add_string buf "]";
  let str = Bytes.to_string (Buffer.to_bytes buf) in
  assert_roundtrip pp_xl xl_to_located_yojson xl_of_located_yojson lst str

let test_array _ctxt =
  assert_roundtrip' pp_xa (xa_to_located_yojson, cmp) (xa_of_located_yojson, (=))
                   [||] "[]";
  assert_roundtrip' pp_xa (xa_to_located_yojson, cmp) (xa_of_located_yojson, (=))
                   [|42; 43|] "[42, 43]"

let test_tuple _ctxt =
  assert_roundtrip' pp_xt (xt_to_located_yojson, cmp) (xt_of_located_yojson, (=))
                   (42, 43) "[42, 43]"

let test_ptyp _ctxt =
  assert_roundtrip (pp_p pp_i1) (p_to_located_yojson i1_to_located_yojson) (p_of_located_yojson i1_of_located_yojson)
                   (Some 42) "42";
  assert_roundtrip (pp_p pp_i1) (p_to_located_yojson i1_to_located_yojson) (p_of_located_yojson i1_of_located_yojson)
                   None "null"

let test_pvar _ctxt =
  begin
    let pv_to_located_yojson = (pv_to_located_yojson, cmp) in
    let pv_of_located_yojson = (pv_of_located_yojson, (=)) in
    let pvd_to_located_yojson = (pvd_to_located_yojson, cmp) in
    let pvd_of_located_yojson = (pvd_of_located_yojson, (=)) in
    assert_roundtrip' pp_pv pv_to_located_yojson pv_of_located_yojson
      `A "[\"A\"]";
    assert_roundtrip' pp_pv pv_to_located_yojson pv_of_located_yojson
      (`B 42) "[\"B\", 42]";
    assert_roundtrip' pp_pv pv_to_located_yojson pv_of_located_yojson
      (`C (42, "foo")) "[\"C\", 42, \"foo\"]";
    assert_roundtrip' pp_pvd pvd_to_located_yojson pvd_of_located_yojson
      `A "[\"A\"]";
    assert_roundtrip' pp_pvd pvd_to_located_yojson pvd_of_located_yojson
      `B "[\"B\"]";
    assert_roundtrip' pp_pvd pvd_to_located_yojson pvd_of_located_yojson
      (`C 1) "[\"C\", 1]"
  end ;
  assert_equal ~printer:(show_error_or pp_pvd)
               (Result.Error (filemod^"pvd"))
               (pvd_of_located_yojson (Ploc.dummy, `List [(Ploc.dummy, `String "D")]))

let test_var _ctxt =
  let v_to_located_yojson = (v_to_located_yojson, cmp) in
  let v_of_located_yojson = (v_of_located_yojson, (=)) in
  assert_roundtrip' pp_v v_to_located_yojson v_of_located_yojson
                   A "[\"A\"]";
  assert_roundtrip' pp_v v_to_located_yojson v_of_located_yojson
                   (B 42) "[\"B\", 42]";
  assert_roundtrip' pp_v v_to_located_yojson v_of_located_yojson
                   (C (42, "foo")) "[\"C\", 42, \"foo\"]"

let test_rec _ctxt =
  assert_roundtrip pp_r r_to_located_yojson r_of_located_yojson
                   {x=42; y="foo"} "{\"x\":42,\"y\":\"foo\"}"

let test_recvar _ctxt =
  let rv_to_located_yojson = (rv_to_located_yojson, cmp) in
  let rv_of_located_yojson = (rv_of_located_yojson, (=)) in
  assert_roundtrip' pp_rv rv_to_located_yojson rv_of_located_yojson
                   RA "[\"RA\"]";
  assert_roundtrip' pp_rv rv_to_located_yojson rv_of_located_yojson
                   (RB 42) "[\"RB\", 42]";
  assert_roundtrip' pp_rv rv_to_located_yojson rv_of_located_yojson
                   (RC(42, "foo")) "[\"RC\", 42, \"foo\"]";
  assert_roundtrip' pp_rv rv_to_located_yojson rv_of_located_yojson
                   (RD{z="foo"}) "[\"RD\", {\"z\": \"foo\"}]"

type geo = {
  lat : float [@located_yojson.key "Latitude"][@sexp.key "Latitude"]  ;
  lon : float [@located_yojson.key "Longitude"][@sexp.key "Longitude"] ;
}
[@@deriving located_yojson, sexp, show]
let test_key _ctxt =
  assert_roundtrip pp_geo geo_to_located_yojson geo_of_located_yojson
                   {lat=35.6895; lon=139.6917}
                   "{\"Latitude\":35.6895,\"Longitude\":139.6917}"
#ifdef PAPPX
; assert_roundtrip_sexp show_geo sexp_of_geo geo_of_sexp
                   {lat=35.6895; lon=139.6917}
                   {|((Latitude 35.6895) (Longitude 139.6917))|}
#else
    ; assert_roundtrip_sexp show_geo sexp_of_geo geo_of_sexp
                   {lat=35.6895; lon=139.6917}
                   {|((lat 35.6895) (lon 139.6917))|}
#endif

let test_field_err _ctxt =
  assert_equal ~printer:(show_error_or pp_geo)
               (Result.Error (filemod^"geo.lat"))
               (geo_of_located_yojson (Ploc.dummy, `Assoc ["Longitude", (Ploc.dummy, `Float 42.0)]))

type id = Pa_ppx_located_yojson.Json.t [@@deriving located_yojson]
let test_id _ctxt =
  assert_roundtrip' pp_json (id_to_located_yojson, cmp) (id_of_located_yojson, equal_error_or cmp)
                   (Ploc.dummy, `Int 42) "42"

type id2 = Sexplib.Sexp.t [@@deriving sexp]
let test_id2 _ctxt =
  assert_roundtrip_sexp Sexplib0.Sexp.to_string_hum sexp_of_id2 id2_of_sexp
                   (Atom "42") "42"

module Custvar1 = struct
type custvar =
| Tea   of string [@located_yojson.name "tea"][@sexp.name "tea"]
| Vodka [@located_yojson.name "vodka"][@sexp.name "vodka"]
[@@deriving located_yojson, show]
let test_custvar _ctxt =
  assert_roundtrip' pp_custvar (custvar_to_located_yojson, cmp) (custvar_of_located_yojson, (=))
                   (Tea "oolong") "[\"tea\", \"oolong\"]";
  assert_roundtrip' pp_custvar (custvar_to_located_yojson, cmp) (custvar_of_located_yojson, (=))
                   Vodka "[\"vodka\"]"
end

module Custvar2 = struct
type custvar =
| Tea   of string [@located_yojson.name "tea"][@sexp.name "tea"]
| Vodka [@located_yojson.name "vodka"][@sexp.name "vodka"]
[@@deriving sexp, show]
#ifdef PAPPX
let test_custvar _ctxt =
  assert_roundtrip_sexp show_custvar sexp_of_custvar custvar_of_sexp
                   (Tea "oolong") "(tea oolong)";
  assert_roundtrip_sexp show_custvar sexp_of_custvar custvar_of_sexp
                   Vodka "vodka"
#else
let test_custvar _ctxt =
  assert_roundtrip_sexp show_custvar sexp_of_custvar custvar_of_sexp
                   (Tea "oolong") "(Tea oolong)";
  assert_roundtrip_sexp show_custvar sexp_of_custvar custvar_of_sexp
                   Vodka "Vodka"
#endif
end

module Custpvar1 = struct
type custpvar =
[ `Tea   of string [@located_yojson.name "tea"][@sexp.name "tea"]
| `Beer  of string * float [@located_yojson.name "beer"][@sexp.name "beer"]
| `Vodka [@located_yojson.name "vodka"][@sexp.name "vodka"]
] [@@deriving located_yojson, show]
let test_custpvar _ctxt =
  let custpvar_to_located_yojson = (custpvar_to_located_yojson, cmp) in
  let custpvar_of_located_yojson = (custpvar_of_located_yojson, (=)) in
  assert_roundtrip' pp_custpvar custpvar_to_located_yojson custpvar_of_located_yojson
                   (`Tea "earl_grey") "[\"tea\", \"earl_grey\"]";
  assert_roundtrip' pp_custpvar custpvar_to_located_yojson custpvar_of_located_yojson
                   (`Beer ("guinness", 3.3)) "[\"beer\", \"guinness\", 3.3]";
  assert_roundtrip' pp_custpvar custpvar_to_located_yojson custpvar_of_located_yojson
                   `Vodka "[\"vodka\"]"
end

module Custpvar2 = struct
type custpvar =
[ `Tea   of string [@located_yojson.name "tea"][@sexp.name "tea"]
| `Beer  of string * float [@located_yojson.name "beer"][@sexp.name "beer"]
| `Vodka [@located_yojson.name "vodka"][@sexp.name "vodka"]
] [@@deriving sexp, show]
#ifdef PAPPX
let test_custpvar _ctxt =
  assert_roundtrip_sexp show_custpvar sexp_of_custpvar custpvar_of_sexp
                   (`Tea "earl_grey") "(tea earl_grey)";
  assert_roundtrip_sexp show_custpvar sexp_of_custpvar custpvar_of_sexp
                   (`Beer ("guinness", 3.3)) "(beer (guinness 3.3))";
  assert_roundtrip_sexp show_custpvar sexp_of_custpvar custpvar_of_sexp
                   `Vodka "vodka"
#else
let test_custpvar _ctxt =
  assert_roundtrip_sexp show_custpvar sexp_of_custpvar custpvar_of_sexp
                   (`Tea "earl_grey") "(Tea earl_grey)";
  assert_roundtrip_sexp show_custpvar sexp_of_custpvar custpvar_of_sexp
                   (`Beer ("guinness", 3.3)) "(Beer (guinness 3.3))";
  assert_roundtrip_sexp show_custpvar sexp_of_custpvar custpvar_of_sexp
                   `Vodka "Vodka"

#endif
end

module Default1 = struct
type default = {
  def : int [@located_yojson.default 42];
} [@@deriving located_yojson, show]
let test_default _ctxt =
  assert_roundtrip pp_default default_to_located_yojson default_of_located_yojson
                   { def = 42 } "{}"
end

module Default2 = struct
type default = {
  def : int [@sexp.default 42][@sexp.sexp_drop_default (=)];
} [@@deriving sexp, show]
let test_default _ctxt =
  assert_roundtrip_sexp show_default sexp_of_default default_of_sexp
                   { def = 42 } "()"
end

module Default3 = struct
type default = {
  def : int option [@located_yojson.default None];
} [@@deriving located_yojson, show]
let test_default _ctxt =
  let default_to_located_yojson = (default_to_located_yojson, cmp) in
  let default_of_located_yojson = (default_of_located_yojson, (=)) in
  assert_roundtrip' pp_default default_to_located_yojson default_of_located_yojson
                   { def = None } "{}"
; assert_roundtrip' pp_default default_to_located_yojson default_of_located_yojson
                   { def = Some 42 } {foo|{"def": 42}|foo}
end

type bidi = int [@@deriving show, to_located_yojson, of_located_yojson, of_sexp, sexp_of]
let test_bidi _ctxt =
  assert_roundtrip pp_bidi bidi_to_located_yojson bidi_of_located_yojson
                   42 "42" ;
  assert_roundtrip_sexp show_bidi sexp_of_bidi bidi_of_sexp
                   42 "42"

let test_shortcut _ctxt =
  assert_roundtrip pp_i1 [%to_located_yojson: int] [%of_located_yojson: int]
                   42 "42" ;
  assert_roundtrip_sexp show_i1 [%sexp_of: int] [%of_sexp: int]
                   42 "42"

module CustomConversions = struct

  module IntMap0 = Map.Make(struct type t = int let compare = Stdlib.Int.compare end)
  type mapEncoding = (int * string) list [@@deriving located_yojson, sexp]
  let map_to_located_yojson m = mapEncoding_to_located_yojson @@ IntMap0.bindings m 
  let map_of_located_yojson json = 
    Result.(match mapEncoding_of_located_yojson json with
              | Ok lst -> Ok (List.fold_left (fun m (k, v) -> IntMap0.add k v m) IntMap0.empty lst)
              | Error s -> Error s)

  let sexp_of_map m = sexp_of_mapEncoding @@ IntMap0.bindings m 
  let map_of_sexp sexp = 
    List.fold_left (fun m (k, v) -> IntMap0.add k v m) IntMap0.empty (mapEncoding_of_sexp sexp)
  module IntMap = struct
    include IntMap0
    let t_of_sexp _ = map_of_sexp
    let sexp_of_t _ = sexp_of_map
  end
  
  type k = string IntMap.t [@located_yojson.to_located_yojson map_to_located_yojson]
                           [@located_yojson.of_located_yojson map_of_located_yojson]
                           [@sexp.sexp_of sexp_of_map]
                           [@sexp.of_sexp map_of_sexp]
                           [@show.printer fun fmt a -> ()]
                           [@@deriving show, located_yojson, sexp]
  let test_bare _ctxt =
    assert_roundtrip pp_k k_to_located_yojson k_of_located_yojson
                     IntMap.(add 6 "foo" @@ empty)
                     {|[[6,"foo"]]|} ;
    assert_roundtrip_sexp show_k sexp_of_k k_of_sexp
                     IntMap.(add 6 "foo" @@ empty)
                     {|((6 "foo"))|}

  type crecord = {
    mapping : string IntMap.t [@located_yojson.to_located_yojson map_to_located_yojson]
                              [@located_yojson.of_located_yojson map_of_located_yojson]
                              [@sexp.sexp_of sexp_of_map]
                              [@sexp.of_sexp map_of_sexp]
                              [@show.printer fun fmt a -> ()]
  } [@@deriving located_yojson, show, sexp]

  let test_record _ctxt =
    assert_roundtrip pp_crecord crecord_to_located_yojson crecord_of_located_yojson
                     IntMap.{ mapping = add 6 "foo" @@ empty }
                     {|{"mapping":[[6,"foo"]]}|} ;
    assert_roundtrip_sexp show_crecord sexp_of_crecord crecord_of_sexp
                     IntMap.{ mapping = add 6 "foo" @@ empty }
                     {|((mapping ((6 foo))))|}
  
  let suite = "Custom conversion attributes" >:::
    [ "test_record"      >:: test_record
    ; "test_bare"        >:: test_bare ]
end

type nostrict = {
  nostrict_field : int;
}
#ifdef PAPPX
[@@deriving show, located_yojson { strict = false }, sexp { strict = false }]
#else
[@@deriving show, located_yojson { strict = false }, sexp] [@@sexp.allow_extra_fields]
#endif
let test_nostrict _ctxt =
  assert_equal ~printer:(show_error_or pp_nostrict)
               (Result.Ok { nostrict_field = 42 })
               (nostrict_of_located_yojson (Ploc.dummy, `Assoc ["nostrict_field", (Ploc.dummy, `Int 42);
                                            "some_other_field", (Ploc.dummy, `Int 43)])) ;
  assert_equal ~printer:show_nostrict
               { nostrict_field = 42 }
               (nostrict_of_sexp (Sexplib.Sexp.of_string "((nostrict_field 42))"))

module Opentype :
  sig
    type 'a opentype = ..
#ifdef PAPPX
  [@@deriving located_yojson, show, eq, sexp]
#else
  [@@deriving located_yojson]
#endif
    type 'a opentype += A of 'a | B of string list
#ifdef PAPPX
    [@@deriving located_yojson, show, eq, sexp]
#else
    [@@deriving located_yojson]
#endif
  end =
  struct
    type 'a opentype = ..
#ifdef PAPPX
  [@@deriving located_yojson, show, eq, sexp]
#else
  [@@deriving located_yojson]
#endif
    type 'a opentype += A of 'a | B of string list
#ifdef PAPPX
    [@@deriving located_yojson, show, eq, sexp]
#else
    [@@deriving located_yojson]
#endif
  end
type 'a Opentype.opentype +=
  | C of 'a Opentype.opentype * float
  | A = Opentype.A
#ifdef PAPPX
   [@@deriving located_yojson, show, eq, sexp]
#else
   [@@deriving located_yojson]
#endif
let rec pp_opentype f fmt = function
  A x -> Format.fprintf fmt "A(%s)" (f x)
| Opentype.B l -> Format.fprintf fmt "B(%s)" (String.concat ", " l)
| C (x, v) ->
    Format.pp_print_string fmt "C(";
    pp_opentype f fmt x;
    Format.fprintf fmt ", %f)" v
| _ -> assert false

let test_opentype _ctxt =
  let pp_ot = pp_opentype string_of_int in
  let to_located_yojson = Opentype.opentype_to_located_yojson i1_to_located_yojson in
  let of_located_yojson = Opentype.opentype_of_located_yojson i1_of_located_yojson in
  let to_located_yojson = (to_located_yojson, cmp) in
  let of_located_yojson = (of_located_yojson, (=)) in
  assert_roundtrip' pp_ot to_located_yojson of_located_yojson
                   (Opentype.A 0) "[\"A\", 0]";
  assert_roundtrip' pp_ot to_located_yojson of_located_yojson
                   (Opentype.B ["one"; "two"]) "[\"B\", [ \"one\", \"two\"] ]";
  assert_roundtrip' pp_ot to_located_yojson of_located_yojson
                   (C (Opentype.A 42, 1.2)) "[\"C\", [\"A\", 42], 1.2]"
#ifdef PAPPX
  ; assert_equal ~printer:(fun x -> x)
    "(Test_ppx_located_yojson.Opentype.A 0)"
    Opentype.(show_opentype pp_i1 (A 0))
  ; assert_equal ~printer:(fun x -> x)
    {|(Test_ppx_located_yojson.Opentype.B ["one"; "two"])|}
    Opentype.(show_opentype pp_i1 (B ["one"; "two"]))
  ; assert_equal ~printer:(fun x -> x)
    "(Test_ppx_located_yojson.C ((Test_ppx_located_yojson.Opentype.A 42), 1.2))"
    (Opentype.show_opentype pp_i1 (C (Opentype.A 42, 1.2)))
  ; assert_bool "A=A" Opentype.(equal_opentype equal_i1 (A 0) (A 0))
  ; assert_bool "B=B" Opentype.(equal_opentype equal_i1 (B ["foo"]) (B ["foo"]))
  ; assert_bool "B1<>B2" (not Opentype.(equal_opentype equal_i1 (B ["foo1"]) (B ["foo2"])))
  ; assert_bool "A<>B" (not Opentype.(equal_opentype equal_i1 (A 0) (B [])))
  ; assert_bool "C=C" (Opentype.equal_opentype equal_i1 (C (Opentype.A 42, 1.2)) (C (Opentype.A 42, 1.2)))
  ; assert_roundtrip_sexp (Opentype.show_opentype pp_i1)
    (Opentype.sexp_of_opentype sexp_of_i1)
    (Opentype.opentype_of_sexp i1_of_sexp)
    Opentype.(A 0) {|(A 0)|}
  ; assert_roundtrip_sexp (Opentype.show_opentype pp_i1)
    (Opentype.sexp_of_opentype sexp_of_i1)
    (Opentype.opentype_of_sexp i1_of_sexp)
    Opentype.(B ["foo"]) {|(B (foo))|}
  ; assert_roundtrip_sexp (Opentype.show_opentype pp_i1)
    (Opentype.sexp_of_opentype sexp_of_i1)
    (Opentype.opentype_of_sexp i1_of_sexp)
    (C (Opentype.A 42, 1.2)) {|(C (A 42) 1.2)|}
#endif

(* This will fail at type-check if we introduce features that increase
   the default generated signatures. It is representative of user code
   (there is plenty in OPAM) that uses our generated signatures, but
   manually implement this restricted function set.

   For example, the unconditional addition of of_located_yojson_exn has broken
   this test. *)
type outer_t = int [@@deriving located_yojson, sexp]
module Automatic_deriving_in_signature_only
  : sig type t [@@deriving located_yojson, sexp] end
  = struct
    type t = int
    let of_located_yojson = outer_t_of_located_yojson
    let to_located_yojson = outer_t_to_located_yojson
    let t_of_sexp = outer_t_of_sexp
    let sexp_of_t = sexp_of_outer_t
  end

module Warnings = struct

  module W34 = struct

    [@@@ocaml.warning "@34"]


    module M1 : sig type u [@@deriving located_yojson, sexp] end = struct
      type internal = int list [@@deriving located_yojson, sexp]
      type u = int list    [@@deriving located_yojson, sexp]
    end
(* the deriver for type [u] supposedly use the derivier of type
       [internal]. Consider for instance the case where [u] is a map,
       and internal is a list of bindings. *)
    module M2 : sig type 'a u [@@deriving located_yojson, sexp] end = struct
      type 'a internal = 'a list [@@deriving located_yojson, sexp]
      type 'a u = 'a list    [@@deriving located_yojson, sexp]
    end

    (* the deriver for type [u] supposedly use the derivier of type
       [internal]. Consider for instance the case where [u] is a map,
       and internal is a list of bindings.  *)
    (* module M1 : sig type 'a u [@@deriving located_yojson] end = struct *)
    (*   type 'a internal = .. [@@deriving located_yojson] (\* Triggers the warning *\) *)
    (*   type 'a internal += A of 'a | B of string list  [@@deriving located_yojson] *)
    (*   type 'a u = 'a list    [@@deriving located_yojson] *)
    (* end *)
end

end


module TestShadowing = struct
  module List = struct
    let map () = ()
  end

  type t = int list [@@deriving located_yojson, sexp]

  module Array = struct
    let to_list () = ()
  end

  module Bytes = struct
    let to_string () = ()
  end

  type v = bytes [@@deriving located_yojson, sexp]

end

module Test_extension_forms = struct
  let _ = [%to_located_yojson: unit], [%of_located_yojson: unit]
  let _ = [%to_located_yojson: int], [%of_located_yojson: int]
  let _ = [%to_located_yojson: int32], [%of_located_yojson: int32]
  let _ = [%to_located_yojson: Int32.t], [%of_located_yojson: Int32.t]
  let _ = [%to_located_yojson: int64], [%of_located_yojson: int64]
  let _ = [%to_located_yojson: Int64.t], [%of_located_yojson: Int64.t]
  let _ = [%to_located_yojson: nativeint], [%of_located_yojson: nativeint]
  let _ = [%to_located_yojson: Nativeint.t], [%of_located_yojson: Nativeint.t]
  let _ = [%to_located_yojson: int64], [%of_located_yojson: int64]
  let _ = [%to_located_yojson: nativeint], [%of_located_yojson: nativeint]
  let _ = [%to_located_yojson: float], [%of_located_yojson: float]
  let _ = [%to_located_yojson: bool], [%of_located_yojson: bool]
  let _ = [%to_located_yojson: char], [%of_located_yojson: char]
  let _ = [%to_located_yojson: string], [%of_located_yojson: string]
  let _ = [%to_located_yojson: bytes], [%of_located_yojson: bytes]
  let _ = [%to_located_yojson: int], [%of_located_yojson: int]
  let _ = [%to_located_yojson: int ref], [%of_located_yojson: int ref]
  let _ = [%to_located_yojson: int option], [%of_located_yojson: int option]
  let _ = [%to_located_yojson: int list], [%of_located_yojson: int list]
  let _ = [%to_located_yojson: int array], [%of_located_yojson: int array]
  let _ = [%to_located_yojson: int * int], [%of_located_yojson: int * int]

  let _ =  [%to_located_yojson: 'a option],
          [%of_located_yojson: 'a option]
  let _ = [%to_located_yojson: [ `A | `B of int | `C of int * string ]],
          [%of_located_yojson: [ `A | `B of int | `C of int * string ]]
  let _ = [%to_located_yojson: [ `C of 'a ]],
          [%of_located_yojson: [ `C of 'a ]]
  let _ = [%to_located_yojson: [ pva | pvb | int pvc ]],
          [%of_located_yojson: [ pva | pvb | int pvc ]]
end

module Test_extension_forms2 = struct
  let _ = [%sexp_of: unit], [%of_sexp: unit]
  let _ = [%sexp_of: int], [%of_sexp: int]
  let _ = [%sexp_of: int32], [%of_sexp: int32]
#ifdef PAPPX
  let _ = [%sexp_of: Int32.t], [%of_sexp: Int32.t]
#endif
  let _ = [%sexp_of: int64], [%of_sexp: int64]
#ifdef PAPPX
  let _ = [%sexp_of: Int64.t], [%of_sexp: Int64.t]
#endif
  let _ = [%sexp_of: nativeint], [%of_sexp: nativeint]
#ifdef PAPPX
  let _ = [%sexp_of: Nativeint.t], [%of_sexp: Nativeint.t]
#endif
  let _ = [%sexp_of: int64], [%of_sexp: int64]
  let _ = [%sexp_of: nativeint], [%of_sexp: nativeint]
  let _ = [%sexp_of: float], [%of_sexp: float]
  let _ = [%sexp_of: bool], [%of_sexp: bool]
  let _ = [%sexp_of: char], [%of_sexp: char]
  let _ = [%sexp_of: string], [%of_sexp: string]
  let _ = [%sexp_of: bytes], [%of_sexp: bytes]
  let _ = [%sexp_of: int], [%of_sexp: int]
  let _ = [%sexp_of: int ref], [%of_sexp: int ref]
  let _ = [%sexp_of: int option], [%of_sexp: int option]
  let _ = [%sexp_of: int list], [%of_sexp: int list]
  let _ = [%sexp_of: int array], [%of_sexp: int array]
  let _ = [%sexp_of: int * int], [%of_sexp: int * int]
#ifdef PAPPX
  let _ =  [%sexp_of: 'a option],
          [%of_sexp: 'a option]
#endif
  let _ = [%sexp_of: [ `A | `B of int | `C of int * string ]],
          [%of_sexp: [ `A | `B of int | `C of int * string ]]
#ifdef PAPPX
  let _ = [%sexp_of: [ `C of 'a ]],
          [%of_sexp: [ `C of 'a ]]
#endif
  let _ = [%sexp_of: [ pva | pvb | int pvc ]],
          [%of_sexp: [ pva | pvb | int pvc ]]
end

(* this test checks that we can derive an _exn deserializer
   even if we use sub-types that are derived with {exn = false} *)
module Test_exn_depends_on_non_exn = struct
  module M : sig
    type t [@@deriving located_yojson { exn = false }]
  end = struct
    type t = int [@@deriving located_yojson { exn = false }]
  end
  open M
  type u = t * t [@@deriving located_yojson { exn = true }]
end

module Test_recursive_polyvariant = struct
  (* Regression test for
     https://github.com/whitequark/ppx_deriving_located_yojson/issues/24 *)
  type a = [ `B of string ]
      [@@deriving of_located_yojson, sexp]
  type b = [a | `C of b list]
      [@@deriving of_located_yojson, sexp]
  type c = [ a | b | `D of b list]
      [@@deriving of_located_yojson, sexp]
  let c_of_located_yojson yj : (c, string) Result.t  = c_of_located_yojson yj
end

type 'a recursive1 = { lhs : string ; rhs : 'a }
 and foo = unit recursive1
 and bar = int recursive1
               [@@deriving show, located_yojson, sexp]

let test_recursive _ctxt =
  assert_roundtrip pp_bar
                   (recursive1_to_located_yojson i1_to_located_yojson)
                   (recursive1_of_located_yojson i1_of_located_yojson)
                   {lhs="x"; rhs=42} "{\"lhs\":\"x\",\"rhs\":42}";
  assert_roundtrip_sexp show_bar
                   (sexp_of_recursive1 sexp_of_i1)
                   (recursive1_of_sexp i1_of_sexp)
                   {lhs="x"; rhs=42} "((lhs x) (rhs 42))";

  assert_roundtrip pp_foo foo_to_located_yojson foo_of_located_yojson
                   {lhs="x"; rhs=()} "{\"lhs\":\"x\",\"rhs\":null}" ;
  assert_roundtrip_sexp show_foo sexp_of_foo foo_of_sexp
                   {lhs="x"; rhs=()} "((lhs x) (rhs ()))" ;

  assert_roundtrip pp_bar bar_to_located_yojson bar_of_located_yojson
                   {lhs="x"; rhs=42} "{\"lhs\":\"x\",\"rhs\":42}" ;
  assert_roundtrip_sexp show_bar sexp_of_bar bar_of_sexp
                   {lhs="x"; rhs=42} "((lhs x) (rhs 42))"

let test_int_redefined ctxt =
  let module M = struct
    type int = Break_things

    let x = [%to_located_yojson: int] 1
    let x' = [%sexp_of: int] 1
  end
  in
  let expected = (Ploc.dummy, `Int 1) in
  assert_equal ~ctxt ~printer:show_json expected M.x ;
  assert_equal ~ctxt ~printer:Sexplib0.Sexp.to_string_hum (Atom"1") M.x'

#ifdef PAPPX
module HT = struct
type ('a, 'b) t = ('a, 'b) Hashtbl.t [@@deriving sexp, located_yojson]
let test_hashtbl_sexp ctxt =
  let canon = function
    Sexplib0.Sexp.List l -> Sexplib0.Sexp.List (List.sort Stdlib.compare l)
  | e -> e
  in
  let open Hashtbl in
  begin
    let ht = create 23 in
    add ht 1 2 ;
    add ht 2 3 ;
    add ht 3 4 ;
    add ht 3 5 ;
    assert_equal ~ctxt ~printer:Sexplib0.Sexp.to_string_hum
      (canon (Sexplib.Sexp.of_string "((1 2) (2 3) (3 4) (3 5))"))
      (canon (sexp_of_t sexp_of_i1 sexp_of_i1 ht))
  end ;
  begin
    let ht = t_of_sexp i1_of_sexp i1_of_sexp  (Sexplib.Sexp.of_string "((1 2) (2 3) (3 5))") in
    assert_equal 2 (Hashtbl.find ht 1)  ;
    assert_equal 3 (Hashtbl.find ht 2)  ;
    assert_equal 5 (Hashtbl.find ht 3)  ;
  end
let test_hashtbl_located_yojson ctxt =
  let canon (json : json) = match json with
    (loc, `List l) -> (loc, `List (List.sort Stdlib.compare l))
  | e -> e
  in
  let open Hashtbl in
  begin
  let (json : json) = (Ploc.dummy, `List[Ploc.dummy, `List[Ploc.dummy, `Int 1; Ploc.dummy, `Int 2]; Ploc.dummy, `List[Ploc.dummy, `Int 2; Ploc.dummy, `Int 3]; Ploc.dummy, `List[Ploc.dummy, `Int 3; Ploc.dummy, `Int 4]; Ploc.dummy, `List[Ploc.dummy, `Int 3; Ploc.dummy, `Int 5]]) in
    let ht = create 23 in
    add ht 1 2 ;
    add ht 2 3 ;
    add ht 3 4 ;
    add ht 3 5 ;
    assert_equal ~ctxt ~printer:show_json
      (canon json)
      (canon (to_located_yojson i1_to_located_yojson i1_to_located_yojson ht))
  end ;
  begin
  let (json : json) = (Ploc.dummy, `List[Ploc.dummy, `List[Ploc.dummy, `Int 1; Ploc.dummy, `Int 2]; Ploc.dummy, `List[Ploc.dummy, `Int 2; Ploc.dummy, `Int 3]; Ploc.dummy, `List[Ploc.dummy, `Int 3; Ploc.dummy, `Int 5]]) in
    let ht = of_located_yojson i1_of_located_yojson i1_of_located_yojson json in
    assert_bool "of_located_yojson" (Rresult.R.is_ok ht) ;
    let ht = Rresult.R.get_ok ht in
    assert_equal ~ctxt ~printer:show_json
      (canon json)
      (canon (to_located_yojson i1_to_located_yojson i1_to_located_yojson ht)) ;

    assert_equal 2 (Hashtbl.find ht 1)  ;
    assert_equal 3 (Hashtbl.find ht 2)  ;
    assert_equal 5 (Hashtbl.find ht 3)  ;
  end
end
#else
module HT = struct
let test_hashtbl_sexp ctxt = ()
let test_hashtbl_located_yojson ctxt = ()
end
#endif

let suite = "Test ppx_located_yojson" >::: [
    "test_unit"      >:: test_unit;
    "test_int"       >:: test_int;
    "test_int_edge"  >:: test_int_edge;
    "test_float"     >:: test_float;
    "test_bool"      >:: test_bool;
    "test_char"      >:: test_char;
    "test_string"    >:: test_string;
    "test_ref"       >:: test_ref;
    "test_option"    >:: test_option;
    "test_poly"      >:: test_poly;
(*
    "test_list"      >:: test_list;
*)
    "test_array"     >:: test_array;
    "test_tuple"     >:: test_tuple;
    "test_ptyp"      >:: test_ptyp;
    "test_pvar"      >:: test_pvar;
    "test_var"       >:: test_var;
    "test_rec"       >:: test_rec;
    "test_recvar"    >:: test_recvar;
    "test_key"       >:: test_key;
    "test_id"        >:: test_id;
    "test_custvar.located_yojson"   >:: Custvar1.test_custvar;
    "test_custvar.sexp"   >:: Custvar2.test_custvar;
    "test_custpvar.located_yojson"  >:: Custpvar1.test_custpvar;
    "test_custpvar.sexp"  >:: Custpvar2.test_custpvar;
    "test_field_err" >:: test_field_err;
    "test_default.located_yojson"   >:: Default1.test_default;
    "test_default.sexp"   >:: Default2.test_default;
    "test_default.located_yojson.2"   >:: Default3.test_default;
    "test_bidi"      >:: test_bidi;
    "test_custom"    >:  CustomConversions.suite;
    "test_shortcut"  >:: test_shortcut;
    "test_nostrict"  >:: test_nostrict;
    "test_opentype"  >:: test_opentype;
    "test_recursive" >:: test_recursive;
    "test_int_redefined" >:: test_int_redefined;
    "test_hashtbl_sexp" >:: HT.test_hashtbl_sexp;
    "test_hashtbl_located_yojson" >:: HT.test_hashtbl_located_yojson;
  ]

#ifdef PAPPX
let _ = ([%of_located_yojson: 'a * 'a] : [%of_located_yojson: 'a * 'a])
let _ = ([%to_located_yojson: 'a * 'a] : [%to_located_yojson: 'a * 'a])
let _ = ([%of_sexp: 'a * 'a] : [%of_sexp: 'a * 'a])
let _ = ([%sexp_of: 'a * 'a] : [%sexp_of: 'a * 'a])
#endif

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
