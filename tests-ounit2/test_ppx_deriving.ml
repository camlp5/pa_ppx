#ifdef PAPPX
let filemod = "Test_ppx_deriving"
#else
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
