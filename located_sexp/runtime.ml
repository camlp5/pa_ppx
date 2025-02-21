(**pp -syntax camlp5o -package sexplib,pa_ppx_runtime,pa_ppx_deriving_plugins.std,pa_ppx_deriving_plugins.sexp,pa_ppx_deriving_plugins.yojson,pa_ppx_import -ppopt -pa_import-package -ppopt sexplib *)

module ST = struct
[%%import: Sexplib__Type.t][@@deriving show]
end

open Pa_ppx_runtime.Exceptions
type t +=
  Of_sexp_error of t * ST.t[@rebind_to Sexplib.Pre_sexp.Of_sexp_error][@name "Of_sexp_error"]
  [@@deriving show]
