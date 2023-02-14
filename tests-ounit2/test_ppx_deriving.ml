#ifdef PAPPX
(** -syntax camlp5o -package $(PAPACKAGES) *)
let filemod = "Test_ppx_deriving"
#else
(** -package $(PPXPACKAGES) *)
let filemod = "Test_ppx_deriving.ml.ppx"
#endif
open OUnit2

let printer = fun x -> x

type a1 = int        [@@deriving show
(* BUG in ppx_deriving wth optional *)
#ifdef PAPPX
, arglebargle { optional = true }
#endif
]
