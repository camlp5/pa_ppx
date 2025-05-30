#ifdef PAPPX
(**pp -syntax camlp5o -package $(PAPACKAGES) *)
let filemod = "Test_deriving_located_sexp."
#else
(**pp -package $(PPXPACKAGES) *)
let filemod = ""
open Ppx_sexp_conv_lib
open Conv
#endif
open OUnit2
#ifdef PAPPX
open Pa_ppx_base
open Pa_ppx_located_sexp
open Pa_ppx_testutils
#else
#endif

Pa_ppx_runtime.Exceptions.Ploc.pp_loc_verbose := true ;;
Pa_ppx_runtime_fat.Exceptions.Ploc.pp_loc_verbose := true ;;

(* Module names below are used in error messages being tested. *)
[@@@warning "-unused-module"]

let eq_loc loc1 loc2 =
  Ploc.first_pos loc1 = Ploc.first_pos loc2 &&
  Ploc.last_pos loc1 = Ploc.last_pos loc2

let assert_raises_exc_at loc f =
  Testutil.assert_raises_exn_pred
    (function
       Ploc.Exc(loc',_) as exc ->
        if not(eq_loc loc loc') then begin
            Fmt.(pf stderr "Locations didn't match: %a <> %a@."
                   Pp_MLast.Ploc.pp loc Pp_MLast.Ploc.pp loc') ;
            false
          end
        else true
     | _ -> false)
    f

let assert_conversion_failure convf pos txt =
  let loc = (Ploc.make_unlined pos) in
  assert_raises_exc_at loc
    (fun _ ->
      txt |> Sexp.of_string |> convf)

module Case_insensitive = struct
  type t =
    | ABC
  [@@deriving located_sexp, show]

  let test ctxt =
    assert_equal ~printer:(fun s -> s) (Sexp.to_string (located_sexp_of_t ABC)) "ABC"
  ; assert_equal ~printer:show ABC (t_of_located_sexp (Pa_sexp.of_string "ABC"))
  ; assert_equal ~printer:show ABC (t_of_located_sexp (Pa_sexp.of_string "aBC"))
  ; assert_conversion_failure t_of_located_sexp (1,6) {| (abc) |}
  ; assert_conversion_failure t_of_located_sexp (1,4) {| abc |}
  ; assert_conversion_failure t_of_located_sexp (1,3) {| () |}
end

module Sum_and_polymorphic_variants = struct
  type poly =
    [ `No_arg
    | `One_arg of int
    | `One_tuple of int * string
    | `Two_args of int * string
    ]
  [@@deriving located_sexp]

  let test1 ctxt =
    List.iter
      (fun (value, sexp_s) ->
        let sexp = Sexp.of_string sexp_s in
        assert_equal ~cmp:Sexp.equal (located_sexp_of_poly value) sexp;
        assert_equal (poly_of_located_sexp sexp) value)
      [ `No_arg, "No_arg"
      ; (`One_arg 1, "(One_arg 1)")
      ; ( `One_tuple (1, "a"),
          "(One_tuple (1 a))" )
      ; (`Two_args (1, "a"), "(Two_args (1 a))")
      ]
  ;;

  let test1b ctxt =
    assert_conversion_failure poly_of_located_sexp (1,3) {| () |}
  ; assert_conversion_failure poly_of_located_sexp (1,6) {| Yadda |}
  ; assert_conversion_failure poly_of_located_sexp (0,9) {|(One_arg)|}
  ; assert_conversion_failure poly_of_located_sexp (0,14) {|(Two_args 1 a)|}
  ; assert_conversion_failure poly_of_located_sexp (11,12) {|(Two_args (a 1))|}

  type nominal =
    | No_arg
    | One_arg of int
    | One_tuple of (int * string)
    | Two_args of int * string
  [@@deriving located_sexp]

  let test2 ctxt =
    List.iter
      (fun (value, sexp_s) ->
        let sexp = Sexp.of_string sexp_s in
        assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_nominal value) sexp;
        assert_equal (nominal_of_located_sexp sexp) value)
      [ No_arg, "No_arg"
      ; (One_arg 1, "(One_arg 1)")
      ; (One_tuple (1, "a"), "(One_tuple (1 a))")
      ; (Two_args (1, "a"), "(Two_args 1 a)")
      ]
  ;;

  let test2b ctxt =
    assert_conversion_failure nominal_of_located_sexp (12,19) {| (One_tuple (1 a b)) |}

end

module Records = struct
  type t =
    { a : int
    ; b : (float * string) list option
    }
  [@@deriving located_sexp]

  let test ctxt =
    let t = { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let sexp = Pa_ppx_located_sexp.Sexp.of_string "((a 2)(b (((1 a)(2.3 b)))))" in
    assert_equal (t_of_located_sexp sexp) t;
    assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
  ;;

  let testb ctxt =
    assert_conversion_failure t_of_located_sexp (1,2) {| a |}
  ; assert_conversion_failure t_of_located_sexp (7,12) {| ((a 2)(w 3)) |}
  ; assert_conversion_failure t_of_located_sexp (1,8) {| ((a 2)) |}

end

module Inline_records = struct
  type t =
    | A of
        { a : int
        ; b : (float * string) list option
        }
    | B of int
  [@@deriving located_sexp]

  let test ctxt =
    let t = A { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let sexp = Pa_ppx_located_sexp.Sexp.of_string "(A (a 2)(b (((1 a)(2.3 b)))))" in
    assert_equal (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
  ; assert_conversion_failure t_of_located_sexp (1,11) {| (A (a 2) ) |}
  ;;
end

module User_specified_conversion = struct
  type my_float = float

  let sexp_of_my_float n = Sexp.Atom (Ploc.dummy, Printf.sprintf "%.4f" n)
  let my_float_of_sexp = Conv.float_of_sexp

  let test ctxt =
    let my_float : my_float = 1.2 in
    let sexp = Sexp.Atom (Ploc.dummy, "1.2000") in
    assert_equal (my_float_of_sexp sexp) my_float;
    assert_equal (sexp_of_my_float my_float) sexp
  ;;
end

#ifndef PAPPX
module Exceptions : sig
  (* no sexp_grammars for exceptions, as they can't be parsed *)
  exception E0 [@@deriving located_sexp]
  exception E1 of string [@@deriving located_sexp]
  exception E2 of string * int [@@deriving located_sexp]
  exception E_tuple of (string * int) [@@deriving located_sexp]

  exception
    E_record of
      { a : string
      ; b : int
      }
  [@@deriving located_sexp]

  val sexp_of_exn : exn -> Sexp.t
  val test : 'a -> unit

end = struct
  exception E0 [@@deriving located_sexp]
  exception E1 of string [@@deriving located_sexp]
  exception E2 of string * int [@@deriving located_sexp]
  exception E_tuple of (string * int) [@@deriving located_sexp]

  exception
    E_record of
      { a : string
      ; b : int
      }
  [@@deriving located_sexp]

#ifdef PAPPX
  let sexp_of_exn = [%located_sexp_of: Pa_ppx_runtime_fat.Exceptions.t]
#else
  let sexp_of_exn = [%located_sexp_of: exn]
#endif
  let test ctxt =
    let cases =
#ifdef PAPPX
      [ E0, "E0"
      ; E1 "a", "(E1 a)"
      ; E2 ("b", 2), "(E2 b 2)"
      ; E_tuple ("c", 3), "(E_tuple(c 3))"
      ; E_record { a = "c"; b = 3 }, "(E_record(a c)(b 3))"
      ]
#else
      [ E0, "test_deriving_sexp.ML.Exceptions.E0"
      ; E1 "a", "(test_deriving_sexp.ML.Exceptions.E1 a)"
      ; E2 ("b", 2), "(test_deriving_sexp.ML.Exceptions.E2 b 2)"
      ; E_tuple ("c", 3), "(test_deriving_sexp.ML.Exceptions.E_tuple(c 3))"
      ; E_record { a = "c"; b = 3 }, "(test_deriving_sexp.ML.Exceptions.E_record(a c)(b 3))"
      ]
#endif
    in
    List.iter
      (fun (exn, sexp_as_str) ->
        let sexp = Sexplib.Sexp.of_string sexp_as_str in
        assert_equal ~printer:Sexp.to_string (sexp_of_exn exn) sexp)
      cases
  ;;
end
#endif
module type Abstract_types_are_only_allowed_in_structures = sig
  type t [@@deriving located_sexp]
end

module Manifest_types = struct
  type a = { t : int }
  type b = a = { t : int } [@@deriving located_sexp]
end

#ifndef PAPPX
module Uses_of_exn = struct
  type t = int * exn [@@deriving located_sexp_of]
end
#endif

module No_unused_rec = struct
  type r = { r : int } [@@deriving located_sexp]
end

module Field_name_should_not_be_rewritten = struct
  open No_unused_rec

  type nonrec r = { r : r }

  let _ = fun (r : r) -> r.r
end

module Polymorphic_variant_inclusion = struct
  type sub1 =
    [ `C1
    | `C2
    ]
  [@@deriving located_sexp]

  type 'b sub2 =
    [ `C4
    | `C5 of 'b
    ]
  [@@deriving located_sexp]

  type ('a, 'b) t = [ sub1 | `C3 of [ `Nested of 'a ] | 'b sub2 | `C6 ] option
  [@@deriving located_sexp]

  let test ctxt =
    let cases : ((string * string, float) t * _) list =
      [ None, "()"
      ; Some `C1, "(C1)"
      ; Some `C2, "(C2)"
      ; Some (`C3 (`Nested ("a", "b"))), "((C3 (Nested (a b))))"
      ; Some `C4, "(C4)"
      ; Some (`C5 1.5), "((C5 1.5))"
      ; Some `C6, "(C6)"
      ]
    in
    List.iter
      (fun (t, sexp_as_str) ->
        let sexp = Sexp.of_string sexp_as_str in
        assert_equal ([%of_located_sexp: (string * string, float) t] sexp) t;
        assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal ([%located_sexp_of: (string * string, float) t] t) sexp)
      cases
  ;;

end


module No_unused_value_warnings : sig end = struct
  module No_warning : sig
    type t = [ `A ] [@@deriving located_sexp]
  end = struct
    type t = [ `A ] [@@deriving located_sexp]
  end

  module Empty = struct end

  module No_warning2 (X : sig
    type t [@@deriving located_sexp]
  end) =
  struct end

  (* this one can't be handled (what if Empty was a functor, huh?) *)
  (* module No_warning3(X : sig type t with sexp end) = Empty *)
  module type S = sig
    type t = [ `A ] [@@deriving located_sexp]
  end

  module No_warning4 : S = struct
    type t = [ `A ] [@@deriving located_sexp]
  end

  module No_warning5 : S = (
    (
    struct
      type t = [ `A ] [@@deriving located_sexp]
    end :
      S) :
      S)

  module Include = struct
    include (
      struct
        type t = int [@@deriving located_sexp]
      end :
        sig
          type t [@@deriving located_sexp]
        end
        with type t := int)
  end
end

module Default = struct
  type t = { a : int [@default 2] } [@@deriving located_sexp, show]

  let test ctxt =
    assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal Sexp.(of_string "((a 1))") (located_sexp_of_t { a = 1 })
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal Sexp.(of_string "((a 2))") (located_sexp_of_t { a = 2 })
  ; assert_equal (t_of_located_sexp Sexp.(of_string "((a 1))")) { a = 1 }
  ; assert_equal (t_of_located_sexp Sexp.(of_string "((a 2))")) { a = 2 }
  ; assert_equal (t_of_located_sexp Sexp.(of_string "()")) { a = 2 }
end

module Type_alias = struct
  (* checking that the [as 'a] is supported and ignored in signatures, that it still
     exports the sexp_of_t__ when needed *)
  module B : sig
    type a = [ `A ]
    type t = [ `A ] as 'a constraint 'a = a [@@deriving located_sexp]
  end = struct
    type a = [ `A ] [@@deriving located_sexp]
    type t = [ `A ] [@@deriving located_sexp]
  end

  let test ctxt =
    assert_equal (Sexp.to_string (B.located_sexp_of_t `A)) "A"
  ; assert_equal `A  (B.t_of_located_sexp (Sexp.of_string "A"))

  module B2 = struct
    type t =
      [ B.t
      | `B
      ]
    [@@deriving located_sexp]
  end

  module C : sig
    type t = int as 'a [@@deriving located_sexp]
  end = struct
    type t = int [@@deriving located_sexp]
  end

  module D : sig
    type t = 'a constraint 'a = int [@@deriving located_sexp]
  end = struct
    type t = int [@@deriving located_sexp]
  end
end

module Tricky_variants = struct
  (* Checking that the generated code compiles (there used to be a problem with subtyping
     constraints preventing proper generalization). *)
  type t = [ `a ] [@@deriving located_sexp]
  type 'a u = [ t | `b of 'a ] * int [@@deriving located_sexp]
end

module Drop_default = struct

  type _t = { a : int } [@@deriving eq]
  type t = _t [@@deriving eq]

  let test ?cr t_of_sexp sexp_of_t =
    begin
    let ( = ) = Sexp.equal in
    assert_bool "1" (Sexp.(of_string "((a 1))") = sexp_of_t { a = 1 });
    assert_bool "2" (Sexp.(of_string "()") = sexp_of_t { a = 2 })
    end;
    begin
    let ( = ) = equal in
    assert_bool "3" (t_of_sexp Sexp.(of_string "((a 1))") = { a = 1 });
    assert_bool "4" (t_of_sexp Sexp.(of_string "((a 2))") = { a = 2 });
    assert_bool "5" (t_of_sexp Sexp.(of_string "()") = { a = 2 })
    end
  ;;

  type my_int = int [@@deriving located_sexp]

  module Builtin = struct
    type t = _t = { a : my_int [@default 2] [@sexp_drop_default] }
    [@@deriving located_sexp]

    let test ctxt = test t_of_located_sexp located_sexp_of_t
  end

  module Poly = struct
    type t = _t = { a : my_int [@default 2] [@sexp_drop_default ( = )] }
    [@@deriving located_sexp]

    let test ctxt = test t_of_located_sexp located_sexp_of_t
  end

  module Equal = struct
    let equal_my_int = [%eq: int]

    type t = _t = { a : my_int [@default 2] [@sexp_drop_default.eq] }
    [@@deriving located_sexp]

    let test ctxt = test t_of_located_sexp located_sexp_of_t
  end

  module Compare = struct
    let compare_my_int = [%ord: int]

    type t = _t = { a : my_int [@default 2] [@sexp_drop_default.ord] }
    [@@deriving located_sexp]

    let test ctxt = test t_of_located_sexp located_sexp_of_t
  end

  module Sexp = struct
    type t = _t = { a : my_int [@default 2] [@sexp_drop_default.sexp] }
    [@@deriving located_sexp]

    let test ctxt = test t_of_located_sexp located_sexp_of_t
  end
end

module Drop_if = struct
  type t = { a : int [@default 2] [@sexp_drop_if fun x -> x mod 2 = 0] }
  [@@deriving located_sexp]

  let test ctxt =
    assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal Sexp.(of_string "((a 1))") (located_sexp_of_t { a = 1 })
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (Sexp.(of_string "()")) (located_sexp_of_t { a = 2 })
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal Sexp.(of_string "((a 3))") (located_sexp_of_t { a = 3 })
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal Sexp.(of_string "()") (located_sexp_of_t { a = 4 })
  ; assert_equal (t_of_located_sexp Sexp.(of_string "((a 1))")) { a = 1 }
  ; assert_equal (t_of_located_sexp Sexp.(of_string "((a 2))")) { a = 2 }
  ; assert_equal (t_of_located_sexp Sexp.(of_string "((a 3))")) { a = 3 }
  ; assert_equal (t_of_located_sexp Sexp.(of_string "((a 4))")) { a = 4 }
  ; assert_equal (t_of_located_sexp Sexp.(of_string "()")) { a = 2 }

  type u =
    { a : int
         [@sexp_drop_if
           fun x ->
             (* pa_type_conv used to drop parens altogether, causing type errors in the
                 following code *)
             let pair = x, 2 in
             match Some pair with
             | None -> true
             | Some (x, y) -> x = y]
    }
  [@@deriving located_sexp]
end

module Omit_nil = struct
  type natural_option = int [@@deriving show]

  let located_sexp_of_natural_option i = if i >= 0 then Conv.sexp_of_int Ploc.dummy i else Conv.sexp_of_unit Ploc.dummy ()

  let natural_option_of_located_sexp = function
    | Sexp.List( _, []) -> -1
    | sexp -> Conv.int_of_sexp sexp
  ;;

  let check show_t sexp_of_t t_of_sexp str t =
    let sexp = Sexp.of_string str in
    assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal sexp (sexp_of_t t);
    assert_equal ~printer:show_t (t_of_sexp sexp) t
  ;;

  type t = { a : natural_option [@omit_nil] } [@@deriving located_sexp, show]

  let test1 ctxt = check show located_sexp_of_t t_of_located_sexp "()" { a = -1 }
  let test2 ctxt = check show located_sexp_of_t t_of_located_sexp "((a 1))" { a = 1 }

  type t2 = A of { a : int list [@omit_nil] } [@@deriving located_sexp, show]

  let test3 ctxt = check show_t2 located_sexp_of_t2 t2_of_located_sexp "(A)" (A { a = [] })
  let test4 ctxt = check show_t2 located_sexp_of_t2 t2_of_located_sexp "(A (a (1)))" (A { a = [ 1 ] })
end

module True_and_false = struct
  type t =
    | True_
    | False_
  [@@deriving located_sexp, show]

  let test1 ctxt =
    assert_equal ~printer:(fun s -> s) (Sexp.to_string (located_sexp_of_t True_)) "True_"
  ; assert_equal ~printer:(fun s -> s) (Sexp.to_string (located_sexp_of_t False_)) "False_"
  ; assert_equal ~printer:show True_ (t_of_located_sexp (Sexp.of_string "True_"))
  ; assert_equal ~printer:show False_ (t_of_located_sexp (Sexp.of_string "False_"))
  ; assert_equal ~printer:show True_ (t_of_located_sexp (Sexp.of_string "true_"))
  ; assert_equal ~printer:show False_ (t_of_located_sexp (Sexp.of_string "false_"))
  type u =
    | True_ of int
    | False_ of int
  [@@deriving located_sexp, show]

  let test2 ctxt =
    assert_equal ~printer:(fun s -> s) (Sexp.to_string (located_sexp_of_u (True_ 1))) "(True_ 1)"
  ; assert_equal ~printer:(fun s -> s) (Sexp.to_string (located_sexp_of_u (False_ 2))) "(False_ 2)"
  ; assert_equal ~printer:show_u (True_ 1) (u_of_located_sexp (Sexp.of_string "(True_ 1)"))
  ; assert_equal ~printer:show_u (False_ 2) (u_of_located_sexp (Sexp.of_string "(False_ 2)"))
  ; assert_equal ~printer:show_u (True_ 1) (u_of_located_sexp (Sexp.of_string "(true_ 1)"))
  ; assert_equal ~printer:show_u (False_ 2) (u_of_located_sexp (Sexp.of_string "(false_ 2)"))

#ifndef PAPPX
  exception True_ [@@deriving located_sexp]

#ifdef PAPPX
  let sexp_of_exn = [%located_sexp_of: Pa_ppx_runtime_fat.Exceptions.t]
#else
  let sexp_of_exn = [%located_sexp_of: exn]
#endif

#ifdef PAPPX
  let test3 ctxt = assert_equal ~printer:(fun s -> s) "True_" (Sexp.to_string (sexp_of_exn True_))
#else
  let test3 ctxt = assert_equal ~printer:(fun s -> s) "test_deriving_sexp.ML.True_and_false.True_" (Sexp.to_string (sexp_of_exn True_))
#endif

  exception False_ of int [@@deriving located_sexp]

#ifdef PAPPX
  let test4 ctxt =
    assert_equal ~printer:(fun s -> s) "(False_ 1)" (Sexp.to_string (sexp_of_exn (False_ 1)))
#else
  let test4 ctxt =
    assert_equal ~printer:(fun s -> s) "(test_deriving_sexp.ML.True_and_false.False_ 1)" (Sexp.to_string (sexp_of_exn (False_ 1)))
#endif
  ;;
#endif
  type v =
    [ `True
    | `False of int
    ]
  [@@deriving located_sexp]

  let test5 ctxt =
    assert_equal ~printer:(fun s -> s) (Sexp.to_string (located_sexp_of_v `True)) "True"
  ; assert_equal ~printer:(fun s -> s) (Sexp.to_string (located_sexp_of_v (`False 2))) "(False 2)"
end

module Record_field_disambiguation = struct
  type a =
    { fl : float
    ; b : b
    }

  and b = { fl : int } [@@deriving located_sexp]
end

module Private = struct
  type t = private int [@@deriving located_sexp_of]
  type ('a, 'b) u = private t [@@deriving located_sexp_of]
  type ('a, 'b, 'c) v = private ('a, 'b) u [@@deriving located_sexp_of]
end

module Nonregular_types = struct
  type 'a nonregular =
    | Leaf of 'a
    | Branch of ('a * 'a) nonregular
  [@@deriving located_sexp]

  type 'a variant = [ `A of 'a ] [@@deriving located_sexp]

  type ('a, 'b) nonregular_with_variant =
    | Branch of ([ | 'a list variant ], 'b) nonregular_with_variant
  [@@deriving located_sexp]
end

module Opaque = struct
  type t = (int[@opaque]) list [@@deriving located_sexp]

  let sexp = Sexp.of_string "(<opaque> <opaque>)"
  let t = [ 1; 2 ]
  let test1 ctxt =
    assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
  ; assert_bool "2"
      (match t_of_located_sexp sexp with
       | _ -> false
       | exception _ -> true)
  ;;

  type u = ([ `A of int ][@opaque]) [@@deriving located_sexp]

  let sexp = Sexp.of_string "<opaque>"
  let u = `A 1
  let test2 ctxt =
    assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_u u) sexp
  ; assert_bool "3"
      (match u_of_located_sexp sexp with
       | _ -> false
       | exception _ -> true)
  ;;
end


module Optional0 = struct
  type t = { x : int option;
             y : int option [@option] } [@@deriving located_sexp, show]

  let sexp = Sexp.of_string "((x (1)) (y 2))"
  let t = { x = Some 1; y = Some 2 }
  let test1 ctxt =
    assert_equal ~printer:show (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
  let sexp' = Sexp.of_string "((x ()))"
  let t' = { x = None  ; y = None }
  let test2 ctxt =
    assert_equal ~printer:show (t_of_located_sexp sexp') t'
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t') sexp'
end

module Optional = struct
  type t = { optional : int option [@option] } [@@deriving located_sexp]

  let sexp = Sexp.of_string "()"
  let t = { optional = None }
  let test1 ctxt =
    assert_equal (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
  let sexp = Sexp.of_string "((optional 5))"
  let t = { optional = Some 5 }
  let test2 ctxt =
    assert_equal (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
end

module Nonempty = struct
  type t =
    { list : int list [@list]
    ; array : int array [@array]
    }
  [@@deriving located_sexp]

  let sexp = Sexp.of_string "()"
  let t = { list = []; array = [||] }
  let test1 ctxt =
    assert_equal (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
  let sexp = Sexp.of_string "((list (1 2 3)) (array (3 2 1)))"
  let t = { list = [ 1; 2; 3 ]; array = [| 3; 2; 1 |] }
  let test2 ctxt =
    assert_equal (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
end

module Boolean = struct
  type t = { no_arg : bool [@bool] } [@@deriving located_sexp, show]

  let sexp = Sexp.of_string "()"
  let t = { no_arg = false }
  let test1 ctxt =
    assert_equal ~printer:show (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp
  let sexp = Sexp.of_string "((no_arg))"
  let badsexp = Sexp.of_string "((no_arg true))"
  let t = { no_arg = true }
  let test2 ctxt =
    assert_equal ~printer:show (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp

  let test3 ctxt =
#ifdef PAPPX
    Testutil.assert_raises_exn_pattern "boolean field.*payload" (fun _ -> t_of_located_sexp badsexp)
  ; assert_conversion_failure t_of_located_sexp (1,14) "((no_arg true))"
#else
  ()
#endif

  type t_allow_extra_fields = { no_arg : bool [@bool] }
  [@@deriving located_sexp] [@@allow_extra_fields]

  let sexp' = Sexp.of_string "((no_arg))"
  let badsexp' = Sexp.of_string "((no_arg true))"
  let t' = { no_arg = true }
  let test4 ctxt =
    assert_equal (t_allow_extra_fields_of_located_sexp sexp') t'
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t_allow_extra_fields t') sexp'

  let test5 ctxt =
#ifdef PAPPX
  Testutil.assert_raises_exn_pattern "boolean field.*payload"
    (fun _ -> t_allow_extra_fields_of_located_sexp badsexp')
#else
  ()
#endif

  let sexp'' = Sexp.of_string "((no_arg)(argle))"
  let sexp''' = Sexp.of_string "((argle)(no_arg))"
  let t'' = { no_arg = true }
  let test6 ctxt =
    assert_equal (t_allow_extra_fields_of_located_sexp sexp'') t''
  ; assert_equal (t_allow_extra_fields_of_located_sexp sexp''') t''
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t_allow_extra_fields t'') sexp'

end

module Inline = struct
  type t = A of int list [@list] [@@deriving located_sexp]

  let sexp = Sexp.of_string "(A 1 2 3)"
  let t = A [ 1; 2; 3 ]
  let test1 ctxt =
    assert_equal (t_of_located_sexp sexp) t
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_t t) sexp

  type u = [ `A of int list [@list] ] [@@deriving located_sexp]

  let sexp = Sexp.of_string "(A 1 2 3)"
  let u = `A [ 1; 2; 3 ]
  let test2 ctxt =
    assert_equal (u_of_located_sexp sexp) u
  ; assert_equal ~printer:Sexp.to_string ~cmp:Sexp.equal (located_sexp_of_u u) sexp
end

module Clash = struct
  (* Same name for type-var and type-name; must be careful when introducing rigid type names. *)
  type 'hey hey = Hey of 'hey [@@deriving located_sexp]
  type 'hey rigid_hey = Hey of 'hey [@@deriving located_sexp]
  type ('foo, 'rigid_foo) foo = Foo of 'foo [@@deriving located_sexp]
  type 'rigid_bar rigid_rigid_bar = Bar [@@deriving located_sexp]
end

#ifdef PAPPX
#else
module Applicative_functor_types = struct
  module Bidirectional_map = struct
    type ('k1, 'k2) t

    module S
        (K1 : sig
           type t
         end)
        (K2 : sig
           type t
         end) =
    struct
      type nonrec t = (K1.t, K2.t) t
    end

    module type Of_sexpable = sig
      type t [@@deriving of_located_sexp]
    end

    let s__t_of_sexp
      (type k1 k2)
      (module K1 : Of_sexpable with type t = k1)
      (module K2 : Of_sexpable with type t = k2)
      (_ : Sexp.t)
      : (k1, k2) t
      =
      assert false
    ;;
  end

  module Int = struct
    type t = int [@@deriving of_located_sexp]
  end

  module String = struct
    type t = string [@@deriving of_located_sexp]
  end

  module M : sig
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_located_sexp]
  end = struct
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_located_sexp]
  end
end
#endif

module Type_extensions = struct
  let _ = ([%located_sexp_of: int] : [%located_sexp_of: int])
  let _ = ([%of_located_sexp: int] : [%of_located_sexp: int])
end

module Allow_extra_fields = struct
  let should_raise f x =
    try
      ignore (f x);
      false
    with
    | _ -> true
  ;;

  module M1 = struct
    type t1 = { a : int } [@@deriving located_sexp]
    type t2 = t1 = { a : int } [@@deriving located_sexp] [@@allow_extra_fields]

    let sexp = Sexp.of_string "((a 1))"
    let sexp_extra = Sexp.of_string "((a 1)(b 2))"
    let test ctxt =
      assert_equal (t2_of_located_sexp sexp) (t2_of_located_sexp sexp_extra)
    ; assert_equal (t1_of_located_sexp sexp) (t2_of_located_sexp sexp)
    ; assert_bool "1" (should_raise t1_of_located_sexp sexp_extra)

  end

  module M2 = struct
    type t1 = A of { a : int list } [@@deriving located_sexp]

    type t2 = t1 = A of { a : int list } [@allow_extra_fields]
    [@@deriving located_sexp]

    let sexp = Sexp.of_string "(A (a (0)))"
    let sexp_extra = Sexp.of_string "(A (a (0))(b 2))"

    let test ctxt =
      assert_equal (t2_of_located_sexp sexp) (t2_of_located_sexp sexp_extra)
    ; assert_equal (t1_of_located_sexp sexp) (t2_of_located_sexp sexp)
    ; assert_bool "1" (should_raise t1_of_located_sexp sexp_extra)
  end
end

let suite = "Test_deriving(located_sexp)" >::: [
      "Sum_and_polymorphic_variants(1b)" >:: Sum_and_polymorphic_variants.test1b
    ; "Case_insensitive" >:: Case_insensitive.test
    ; "Sum_and_polymorphic_variants(2)" >:: Sum_and_polymorphic_variants.test2
    ; "Sum_and_polymorphic_variants(2b)" >:: Sum_and_polymorphic_variants.test2b
    ; "Records" >:: Records.test
    ; "Records.b" >:: Records.testb
    ; "Inline_records" >:: Inline_records.test
    ; "User_specified_conversion" >:: User_specified_conversion.test
#ifndef PAPPX
    ; "Exceptions" >:: Exceptions.test
#endif
    ; "Polymorphic_variant_inclusion" >:: Polymorphic_variant_inclusion.test
    ; "Default" >:: Default.test
    ; "Type_alias" >:: Type_alias.test
#ifdef PAPPX
    ; "Drop_default.Builtin" >:: Drop_default.Builtin.test
    ; "Drop_default.Poly" >:: Drop_default.Poly.test
    ; "Drop_default.Equal" >:: Drop_default.Equal.test
    ; "Drop_default.Compare" >:: Drop_default.Compare.test
    ; "Drop_default.Sexp" >:: Drop_default.Sexp.test
#else
#endif
    ; "Drop_if" >:: Drop_if.test
    ; "Omit_nil(1)" >:: Omit_nil.test1
    ; "Omit_nil(2)" >:: Omit_nil.test2
    ; "Omit_nil(3)" >:: Omit_nil.test3
    ; "Omit_nil(4)" >:: Omit_nil.test4
    ; "True_and_false(1)" >:: True_and_false.test1
    ; "True_and_false(2)" >:: True_and_false.test2
#ifndef PAPPX
    ; "True_and_false(3)" >:: True_and_false.test3
    ; "True_and_false(4)" >:: True_and_false.test4
#endif
    ; "True_and_false(5)" >:: True_and_false.test5
    ; "Opaque(1)" >:: Opaque.test1
    ; "Opaque(2)" >:: Opaque.test2
    ; "Optional0(1)" >:: Optional0.test1
    ; "Optional0(2)" >:: Optional0.test2
    ; "Optional(1)" >:: Optional.test1
    ; "Optional(2)" >:: Optional.test2
    ; "Nonempty(1)" >:: Nonempty.test1
    ; "Nonempty(2)" >:: Nonempty.test2
    ; "Boolean(1)" >:: Boolean.test1
    ; "Boolean(2)" >:: Boolean.test2
    ; "Boolean(3)" >:: Boolean.test3
    ; "Boolean(4)" >:: Boolean.test4
    ; "Boolean(5)" >:: Boolean.test5
    ; "Boolean(6)" >:: Boolean.test6
    ; "Inline(1)" >:: Inline.test1
    ; "Inline(2)" >:: Inline.test2
    ; "Allow_extra_fields.M1" >:: Allow_extra_fields.M1.test
    ; "Allow_extra_fields.M2" >:: Allow_extra_fields.M2.test
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
