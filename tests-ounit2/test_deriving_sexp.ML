#ifdef PAPPX
(**pp -syntax camlp5o -package $(PAPACKAGES) -ppopt -pa_ppx-runtime -ppopt Pa_ppx_runtime_fat *)
let filemod = "Test_deriving_sexp."
#else
(**pp -package $(PPXPACKAGES) *)
let filemod = ""
open Ppx_sexp_conv_lib
open Conv
#endif
open Sexplib
open Conv
open OUnit2

(* Module names below are used in error messages being tested. *)
[@@@warning "-unused-module"]

module Sum_and_polymorphic_variants = struct
  type poly =
    [ `No_arg
    | `One_arg of int
    | `One_tuple of int * string
    | `Two_args of int * string
    ]
  [@@deriving sexp]

  let test1 ctxt =
    List.iter
      (fun (value, sexp) ->
        assert_equal (sexp_of_poly value) sexp;
        assert_equal (poly_of_sexp sexp) value)
      [ `No_arg, Sexp.Atom "No_arg"
      ; (`One_arg 1, Sexp.(List [ Atom "One_arg"; Atom "1" ]))
      ; ( `One_tuple (1, "a")
        , Sexp.(List [ Atom "One_tuple"; List [ Atom "1"; Atom "a" ] ]) )
      ; (`Two_args (1, "a"), Sexp.(List [ Atom "Two_args"; List [ Atom "1"; Atom "a" ] ]))
      ]
  ;;

  type nominal =
    | No_arg
    | One_arg of int
    | One_tuple of (int * string)
    | Two_args of int * string
  [@@deriving sexp]

  let test2 ctxt =
    List.iter
      (fun (value, sexp) ->
        assert_equal (sexp_of_nominal value) sexp;
        assert_equal (nominal_of_sexp sexp) value)
      [ No_arg, Sexp.Atom "No_arg"
      ; (One_arg 1, Sexp.(List [ Atom "One_arg"; Atom "1" ]))
      ; (One_tuple (1, "a"), Sexp.(List [ Atom "One_tuple"; List [ Atom "1"; Atom "a" ] ]))
      ; (Two_args (1, "a"), Sexp.(List [ Atom "Two_args"; Atom "1"; Atom "a" ]))
      ]
  ;;
end

module Records = struct
  type t =
    { a : int
    ; b : (float * string) list option
    }
  [@@deriving sexp]

  let test ctxt =
    let t = { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let sexp = Sexplib.Sexp.of_string "((a 2)(b (((1 a)(2.3 b)))))" in
    assert_equal (t_of_sexp sexp) t;
    assert_equal (sexp_of_t t) sexp
  ;;

end

module Inline_records = struct
  type t =
    | A of
        { a : int
        ; b : (float * string) list option
        }
    | B of int
  [@@deriving sexp]

  let test ctxt =
    let t = A { a = 2; b = Some [ 1., "a"; 2.3, "b" ] } in
    let sexp = Sexplib.Sexp.of_string "(A (a 2)(b (((1 a)(2.3 b)))))" in
    assert_equal (t_of_sexp sexp) t;
    assert_equal (sexp_of_t t) sexp
  ;;
end

module User_specified_conversion = struct
  type my_float = float

  let sexp_of_my_float n = Sexp.Atom (Printf.sprintf "%.4f" n)
  let my_float_of_sexp = float_of_sexp

  let test ctxt =
    let my_float : my_float = 1.2 in
    let sexp = Sexp.Atom "1.2000" in
    assert_equal (my_float_of_sexp sexp) my_float;
    assert_equal (sexp_of_my_float my_float) sexp
  ;;
end

module Exceptions : sig
  (* no sexp_grammars for exceptions, as they can't be parsed *)
  exception E0 [@@deriving sexp]
  exception E1 of string [@@deriving sexp]
  exception E2 of string * int [@@deriving sexp]
  exception E_tuple of (string * int) [@@deriving sexp]

  exception
    E_record of
      { a : string
      ; b : int
      }
  [@@deriving sexp]

  val sexp_of_exn : exn -> Sexp.t
  val test : 'a -> unit

end = struct
  exception E0 [@@deriving sexp]
  exception E1 of string [@@deriving sexp]
  exception E2 of string * int [@@deriving sexp]
  exception E_tuple of (string * int) [@@deriving sexp]

  exception
    E_record of
      { a : string
      ; b : int
      }
  [@@deriving sexp]

#ifdef PAPPX
  let sexp_of_exn = [%sexp_of: Pa_ppx_runtime_fat.Exceptions.t]
#else
  let sexp_of_exn = [%sexp_of: exn]
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

module type Abstract_types_are_only_allowed_in_structures = sig
  type t [@@deriving sexp]
end

module Manifest_types = struct
  type a = { t : int }
  type b = a = { t : int } [@@deriving sexp]
end

module Uses_of_exn = struct
  type t = int * exn [@@deriving sexp_of]
end

module No_unused_rec = struct
  type r = { r : int } [@@deriving sexp]
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
  [@@deriving sexp]

  type 'b sub2 =
    [ `C4
    | `C5 of 'b
    ]
  [@@deriving sexp]

  type ('a, 'b) t = [ sub1 | `C3 of [ `Nested of 'a ] | 'b sub2 | `C6 ] option
  [@@deriving sexp]

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
        let sexp = Sexplib.Sexp.of_string sexp_as_str in
        assert_equal ([%of_sexp: (string * string, float) t] sexp) t;
        assert_equal ([%sexp_of: (string * string, float) t] t) sexp)
      cases
  ;;

end


module No_unused_value_warnings : sig end = struct
  module No_warning : sig
    type t = [ `A ] [@@deriving sexp]
  end = struct
    type t = [ `A ] [@@deriving sexp]
  end

  module Empty = struct end

  module No_warning2 (X : sig
    type t [@@deriving sexp]
  end) =
  struct end

  (* this one can't be handled (what if Empty was a functor, huh?) *)
  (* module No_warning3(X : sig type t with sexp end) = Empty *)
  module type S = sig
    type t = [ `A ] [@@deriving sexp]
  end

  module No_warning4 : S = struct
    type t = [ `A ] [@@deriving sexp]
  end

  module No_warning5 : S = (
    (
    struct
      type t = [ `A ] [@@deriving sexp]
    end :
      S) :
      S)

  module Include = struct
    include (
      struct
        type t = int [@@deriving sexp]
      end :
        sig
          type t [@@deriving sexp]
        end
        with type t := int)
  end
end

module Default = struct
  type t = { a : int [@default 2] } [@@deriving sexp, show]

  let test ctxt =
    assert_equal ~printer:Sexp.to_string Sexp.(List [ List [ Atom "a"; Atom "1" ] ]) (sexp_of_t { a = 1 })
  ; assert_equal ~printer:Sexp.to_string Sexp.(List [ List [ Atom "a"; Atom "2" ] ]) (sexp_of_t { a = 2 })
  ; assert_equal (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "1" ] ])) { a = 1 }
  ; assert_equal (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "2" ] ])) { a = 2 }
  ; assert_equal (t_of_sexp Sexp.(List [])) { a = 2 }
end

module Type_alias = struct
  (* checking that the [as 'a] is supported and ignored in signatures, that it still
     exports the sexp_of_t__ when needed *)
  module B : sig
    type a = [ `A ]
    type t = [ `A ] as 'a constraint 'a = a [@@deriving sexp]
  end = struct
    type a = [ `A ] [@@deriving sexp]
    type t = [ `A ] [@@deriving sexp]
  end

  let test ctxt =
    assert_equal (Sexp.to_string (B.sexp_of_t `A)) "A"
  ; assert_equal `A  (B.t_of_sexp (Sexplib.Sexp.of_string "A"))

  module B2 = struct
    type t =
      [ B.t
      | `B
      ]
    [@@deriving sexp]
  end

  module C : sig
    type t = int as 'a [@@deriving sexp]
  end = struct
    type t = int [@@deriving sexp]
  end

  module D : sig
    type t = 'a constraint 'a = int [@@deriving sexp]
  end = struct
    type t = int [@@deriving sexp]
  end
end

module Tricky_variants = struct
  (* Checking that the generated code compiles (there used to be a problem with subtyping
     constraints preventing proper generalization). *)
  type t = [ `a ] [@@deriving sexp]
  type 'a u = [ t | `b of 'a ] * int [@@deriving sexp]
end

module Drop_default = struct

  type _t = { a : int } [@@deriving eq]
  type t = _t [@@deriving eq]

  let test ?cr t_of_sexp sexp_of_t =
    begin
    let ( = ) = Sexp.equal in
    assert_bool "1" (Sexp.(List [ List [ Atom "a"; Atom "1" ] ]) = sexp_of_t { a = 1 });
    assert_bool "2" (Sexp.(List []) = sexp_of_t { a = 2 })
    end;
    begin
    let ( = ) = equal in
    assert_bool "3" (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "1" ] ]) = { a = 1 });
    assert_bool "4" (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "2" ] ]) = { a = 2 });
    assert_bool "5" (t_of_sexp Sexp.(List []) = { a = 2 })
    end
  ;;

  type my_int = int [@@deriving sexp]

  module Builtin = struct
    type t = _t = { a : my_int [@default 2] [@sexp_drop_default] }
    [@@deriving sexp]

    let test ctxt = test t_of_sexp sexp_of_t
  end

  module Poly = struct
    type t = _t = { a : my_int [@default 2] [@sexp_drop_default ( = )] }
    [@@deriving sexp]

    let test ctxt = test t_of_sexp sexp_of_t
  end

  module Equal = struct
    let equal_my_int = [%eq: int]

    type t = _t = { a : my_int [@default 2] [@sexp_drop_default.eq] }
    [@@deriving sexp]

    let test ctxt = test t_of_sexp sexp_of_t
  end

  module Compare = struct
    let compare_my_int = [%ord: int]

    type t = _t = { a : my_int [@default 2] [@sexp_drop_default.ord] }
    [@@deriving sexp]

    let test ctxt = test t_of_sexp sexp_of_t
  end

  module Sexp = struct
    type t = _t = { a : my_int [@default 2] [@sexp_drop_default.sexp] }
    [@@deriving sexp]

    let test ctxt = test t_of_sexp sexp_of_t
  end
end

module Drop_if = struct
  type t = { a : int [@default 2] [@sexp_drop_if fun x -> x mod 2 = 0] }
  [@@deriving sexp]

  let test ctxt =
    assert_equal ~printer:Sexp.to_string Sexp.(List [ List [ Atom "a"; Atom "1" ] ]) (sexp_of_t { a = 1 })
  ; assert_equal ~printer:Sexp.to_string (Sexp.(List [])) (sexp_of_t { a = 2 })
  ; assert_equal ~printer:Sexp.to_string Sexp.(List [ List [ Atom "a"; Atom "3" ] ]) (sexp_of_t { a = 3 })
  ; assert_equal ~printer:Sexp.to_string Sexp.(List []) (sexp_of_t { a = 4 })
  ; assert_equal (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "1" ] ])) { a = 1 }
  ; assert_equal (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "2" ] ])) { a = 2 }
  ; assert_equal (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "3" ] ])) { a = 3 }
  ; assert_equal (t_of_sexp Sexp.(List [ List [ Atom "a"; Atom "4" ] ])) { a = 4 }
  ; assert_equal (t_of_sexp Sexp.(List [])) { a = 2 }

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
  [@@deriving sexp]
end

module Omit_nil = struct
  type natural_option = int

  let sexp_of_natural_option i = if i >= 0 then sexp_of_int i else sexp_of_unit ()

  let natural_option_of_sexp = function
    | Sexp.List [] -> -1
    | sexp -> int_of_sexp sexp
  ;;

  let natural_option_sexp_grammar : natural_option Sexplib0.Sexp_grammar.t =
    { untyped = Union [ List Empty; Integer ] }
  ;;

  let check sexp_of_t t_of_sexp str t =
    let sexp = Sexplib.Sexp.of_string str in
    assert_equal sexp (sexp_of_t t);
    assert_equal (t_of_sexp sexp) t
  ;;

  type t = { a : natural_option [@sexp.omit_nil] } [@@deriving sexp]

  let test1 ctxt = check sexp_of_t t_of_sexp "()" { a = -1 }
  let test2 ctxt = check sexp_of_t t_of_sexp "((a 1))" { a = 1 }

  type t2 = A of { a : int list [@sexp.omit_nil] } [@@deriving sexp]

  let test3 ctxt = check sexp_of_t2 t2_of_sexp "(A)" (A { a = [] })
  let test4 ctxt = check sexp_of_t2 t2_of_sexp "(A (a (1)))" (A { a = [ 1 ] })
end

module True_and_false = struct
  type t =
    | True
    | False
  [@@deriving sexp]

  let test1 ctxt =
    assert_equal (Sexp.to_string (sexp_of_t True)) "True"
  ; assert_equal (Sexp.to_string (sexp_of_t False)) "False"
  ; assert_equal True (t_of_sexp (Sexplib.Sexp.of_string "True"))
  ; assert_equal False (t_of_sexp (Sexplib.Sexp.of_string "False"))
  ; assert_equal True (t_of_sexp (Sexplib.Sexp.of_string "true"))
  ; assert_equal False (t_of_sexp (Sexplib.Sexp.of_string "false"))

  type u =
    | True of int
    | False of int
  [@@deriving sexp]

  let test2 ctxt =
    assert_equal (Sexp.to_string (sexp_of_u (True 1))) "(True 1)"
  ; assert_equal (Sexp.to_string (sexp_of_u (False 2))) "(False 2)"
  ; assert_equal (True 1) (u_of_sexp (Sexplib.Sexp.of_string "(True 1)"))
  ; assert_equal (False 2) (u_of_sexp (Sexplib.Sexp.of_string "(False 2)"))
  ; assert_equal (True 1) (u_of_sexp (Sexplib.Sexp.of_string "(true 1)"))
  ; assert_equal (False 2) (u_of_sexp (Sexplib.Sexp.of_string "(false 2)"))

  exception True [@@deriving sexp]

  let test3 ctxt = assert_equal "ppx_sexp_test.ml.True_and_false.True" (Sexp.to_string (sexp_of_exn True))

  exception False of int [@@deriving sexp]

  let test4 ctxt =
    assert_equal "(ppx_sexp_test.ml.True_and_false.False 1)" (Sexp.to_string (sexp_of_exn (False 1)))
  ;;

  type v =
    [ `True
    | `False of int
    ]
  [@@deriving sexp]

  let test5 ctxt =
    assert_equal (Sexp.to_string (sexp_of_v `True)) "True"
  ; assert_equal (Sexp.to_string (sexp_of_v (`False 2))) "(False 2)"
end

module Record_field_disambiguation = struct
  type a =
    { fl : float
    ; b : b
    }

  and b = { fl : int } [@@deriving sexp]
end

module Private = struct
  type t = private int [@@deriving sexp_of]
  type ('a, 'b) u = private t [@@deriving sexp_of]
  type ('a, 'b, 'c) v = private ('a, 'b) u [@@deriving sexp_of]
end

module Nonregular_types = struct
  type 'a nonregular =
    | Leaf of 'a
    | Branch of ('a * 'a) nonregular
  [@@deriving sexp]

  type 'a variant = [ `A of 'a ] [@@deriving sexp]

  type ('a, 'b) nonregular_with_variant =
    | Branch of ([ | 'a list variant ], 'b) nonregular_with_variant
  [@@deriving sexp]
end

module Opaque = struct
  type t = (int[@sexp.opaque]) list [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "(<opaque> <opaque>)"
  let t = [ 1; 2 ]
  let test1 ctxt =
    assert_equal (sexp_of_t t) sexp
  ; assert_bool "2"
      (match t_of_sexp sexp with
       | _ -> false
       | exception _ -> true)
  ;;

  type u = ([ `A of int ][@sexp.opaque]) [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "<opaque>"
  let u = `A 1
  let test2 ctxt =
    assert_equal (sexp_of_u u) sexp
  ; assert_bool "3"
      (match u_of_sexp sexp with
       | _ -> false
       | exception _ -> true)
  ;;
end

module Optional = struct
  type t = { optional : int option [@sexp.option] } [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "()"
  let t = { optional = None }
  let test1 ctxt =
    assert_equal (t_of_sexp sexp) t
  ; assert_equal (sexp_of_t t) sexp
  let sexp = Sexplib.Sexp.of_string "((optional 5))"
  let t = { optional = Some 5 }
  let test2 ctxt =
    assert_equal (t_of_sexp sexp) t
  ; assert_equal (sexp_of_t t) sexp
end

module Nonempty = struct
  type t =
    { list : int list [@sexp.list]
    ; array : int array [@sexp.array]
    }
  [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "()"
  let t = { list = []; array = [||] }
  let test1 ctxt =
    assert_equal (t_of_sexp sexp) t
  ; assert_equal (sexp_of_t t) sexp
  let sexp = Sexplib.Sexp.of_string "((list (1 2 3)) (array (3 2 1)))"
  let t = { list = [ 1; 2; 3 ]; array = [| 3; 2; 1 |] }
  let test2 ctxt =
    assert_equal (t_of_sexp sexp) t
    ; assert_equal (sexp_of_t t) sexp
end

module Boolean = struct
  type t = { no_arg : bool [@sexp.bool] } [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "()"
  let t = { no_arg = false }
  let test1 ctxt =
    assert_equal (t_of_sexp sexp) t
  ; assert_equal (sexp_of_t t) sexp
  let sexp = Sexplib.Sexp.of_string "((no_arg))"
  let t = { no_arg = true }
  let test2 ctxt =
    assert_equal (t_of_sexp sexp) t
  ; assert_equal (sexp_of_t t) sexp

  type t_allow_extra_fields = { no_arg : bool [@sexp.bool] }
  [@@deriving sexp] [@@sexp.allow_extra_fields]

end

module Inline = struct
  type t = A of int list [@sexp.list] [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "(A 1 2 3)"
  let t = A [ 1; 2; 3 ]
  let test1 ctxt =
    assert_equal (t_of_sexp sexp) t
  ; assert_equal (sexp_of_t t) sexp

  type u = [ `A of int list [@sexp.list] ] [@@deriving sexp]

  let sexp = Sexplib.Sexp.of_string "(A 1 2 3)"
  let u = `A [ 1; 2; 3 ]
  let test2 ctxt =
    assert_equal (u_of_sexp sexp) u
  ; assert_equal (sexp_of_u u) sexp
end

module Clash = struct
  (* Same name for type-var and type-name; must be careful when introducing rigid type names. *)
  type 'hey hey = Hey of 'hey [@@deriving sexp]
  type 'hey rigid_hey = Hey of 'hey [@@deriving sexp]
  type ('foo, 'rigid_foo) foo = Foo of 'foo [@@deriving sexp]
  type 'rigid_bar rigid_rigid_bar = Bar [@@deriving sexp]
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
      type t [@@deriving of_sexp]
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
    type t = int [@@deriving of_sexp]
  end

  module String = struct
    type t = string [@@deriving of_sexp]
  end

  module M : sig
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_sexp]
  end = struct
    type t = Bidirectional_map.S(String)(Int).t [@@deriving of_sexp]
  end
end
#endif

module Type_extensions = struct
  let _ = ([%sexp_of: int] : [%sexp_of: int])
  let _ = ([%of_sexp: int] : [%of_sexp: int])
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
    type t1 = { a : int } [@@deriving sexp]
    type t2 = t1 = { a : int } [@@deriving sexp] [@@sexp.allow_extra_fields]

    let sexp = Sexplib.Sexp.of_string "((a 1))"
    let sexp_extra = Sexplib.Sexp.of_string "((a 1)(b 2))"
    let test ctxt =
      assert_equal (t2_of_sexp sexp) (t2_of_sexp sexp_extra)
    ; assert_equal (t1_of_sexp sexp) (t2_of_sexp sexp)
    ; assert_bool "1" (should_raise t1_of_sexp sexp_extra)

  end

  module M2 = struct
    type t1 = A of { a : int list } [@@deriving sexp]

    type t2 = t1 = A of { a : int list } [@sexp.allow_extra_fields]
    [@@deriving sexp]

    let sexp = Sexplib.Sexp.of_string "(A (a (0)))"
    let sexp_extra = Sexplib.Sexp.of_string "(A (a (0))(b 2))"

    let test ctxt =
      assert_equal (t2_of_sexp sexp) (t2_of_sexp sexp_extra)
    ; assert_equal (t1_of_sexp sexp) (t2_of_sexp sexp)
    ; assert_bool "1" (should_raise t1_of_sexp sexp_extra)
  end
end

let suite = "Test_deriving(sexp)" >::: [
      "Sum_and_polymorphic_variants(1)" >:: Sum_and_polymorphic_variants.test1
    ; "Sum_and_polymorphic_variants(2)" >:: Sum_and_polymorphic_variants.test2
    ; "Records" >:: Records.test
    ; "Inline_records" >:: Inline_records.test
    ; "User_specified_conversion" >:: User_specified_conversion.test
    ; "Exceptions" >:: Exceptions.test
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
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()
