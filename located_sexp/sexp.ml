(**pp -syntax camlp5o -package sexplib,pa_ppx_import *)

[%%import: Sexp0.t]

let loc_of_sexp = Sexp0.loc_of_sexp
let to_sexplib_sexp e =
  let module SS = Sexplib.Sexp in
  let rec convrec = function
        Atom(_, s) -> SS.Atom s
      | List(_, l) -> SS.List (List.map convrec l)
  in convrec e

let of_sexplib_sexp loc e =
  let module SS = Sexplib.Sexp in
  let rec convrec = function
        SS.Atom s -> Atom(loc, s)
      | List l -> List(loc, List.map convrec l)
  in convrec e

let to_string se = Sexplib.Sexp.to_string (to_sexplib_sexp se)
let of_string = Pa_sexp.of_string
let equal a b = Sexplib.Sexp.equal (to_sexplib_sexp a) (to_sexplib_sexp b)

let input_sexp = Pa_sexp.input_sexp
let load_sexp = Pa_sexp.load_sexp
let pp_hum pps se = Sexplib.Sexp.pp_hum pps (to_sexplib_sexp se)

