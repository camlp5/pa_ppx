#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|# Specifications for the "pa_ppx_deriving_plugins" preprocessor:
requires = "camlp5,pa_ppx_deriving"
version = "%s"
description = "pa_ppx_deriving_plugins support"

package "std" (
# For linking
  package "link" (
requires = "camlp5,pa_ppx_deriving.link"
archive(byte) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"
archive(native) = "pa_deriving_show.cmx pa_deriving_eq.cmx pa_deriving_ord.cmx pa_deriving_enum.cmx pa_deriving_iter.cmx pa_deriving_map.cmx pa_deriving_fold.cmx pa_deriving_make.cmx"
  )
  requires = "pa_ppx_runtime"

# For the toploop:
requires(toploop) = "camlp5,pa_ppx_deriving"
archive(byte,toploop) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
  archive(syntax,preprocessor,-native) = "pa_deriving_show.cmo pa_deriving_eq.cmo pa_deriving_ord.cmo pa_deriving_enum.cmo pa_deriving_iter.cmo pa_deriving_map.cmo pa_deriving_fold.cmo pa_deriving_make.cmo"
  archive(syntax,preprocessor,native) = "pa_deriving_show.cmx pa_deriving_eq.cmx pa_deriving_ord.cmx pa_deriving_enum.cmx pa_deriving_iter.cmx pa_deriving_map.cmx pa_deriving_fold.cmx pa_deriving_make.cmx"

)

package "show" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_show.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_show.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_show.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_show.cmo"
  archive(native) = "pa_deriving_show.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "eq" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_eq.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_eq.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_eq.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_eq.cmo"
  archive(native) = "pa_deriving_eq.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "ord" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_ord.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_ord.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_ord.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_ord.cmo"
  archive(native) = "pa_deriving_ord.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "enum" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_enum.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_enum.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_enum.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_enum.cmo"
  archive(native) = "pa_deriving_enum.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "iter" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_iter.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_iter.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_iter.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_iter.cmo"
  archive(native) = "pa_deriving_iter.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "map" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_map.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_map.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_map.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_map.cmo"
  archive(native) = "pa_deriving_map.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "fold" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_fold.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_fold.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_fold.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_fold.cmo"
  archive(native) = "pa_deriving_fold.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "make" (
  requires(toploop) = "camlp5,pa_ppx_deriving"
  archive(toploop) = "pa_deriving_make.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_make.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_make.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_make.cmo"
  archive(native) = "pa_deriving_make.cmx"
  )
  requires = "pa_ppx_runtime"
)

package "yojson" (
  requires(toploop) = "camlp5,pa_ppx_deriving,yojson"
  archive(toploop) = "pa_deriving_yojson.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving"
    archive(syntax,preprocessor,-native) = "pa_deriving_yojson.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_yojson.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link"
  archive(byte) = "pa_deriving_yojson.cmo"
  archive(native) = "pa_deriving_yojson.cmx"
  )
  requires = "pa_ppx_runtime,yojson"
)

package "sexp" (
  requires(toploop) = "camlp5,pa_ppx_deriving,pa_ppx_deriving_plugins.eq,pa_ppx_deriving_plugins.ord"
  archive(toploop) = "pa_deriving_sexp.cmo"

    requires(syntax,preprocessor) = "camlp5,pa_ppx_deriving,pa_ppx_deriving_plugins.eq,pa_ppx_deriving_plugins.ord"
    archive(syntax,preprocessor,-native) = "pa_deriving_sexp.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_sexp.cmx"

  package "link" (
  requires = "camlp5,pa_ppx_deriving.link,pa_ppx_deriving_plugins.eq.link,pa_ppx_deriving_plugins.ord.link"
  archive(byte) = "pa_deriving_sexp.cmo"
  archive(native) = "pa_deriving_sexp.cmx"
  )
  requires = "pa_ppx_runtime,pa_ppx_deriving_plugins.eq,pa_ppx_deriving_plugins.ord"
)

package "params" (
  requires(toploop) = "fmt,camlp5,pa_ppx_deriving,pa_ppx_base,camlp5.parser_quotations,camlp5.pa_r"
  archive(toploop) = "pa_deriving_params.cmo"

    requires(syntax,preprocessor) = "fmt,camlp5,pa_ppx_deriving,pa_ppx_base.link,camlp5.parser_quotations,camlp5.pa_r"
    archive(syntax,preprocessor,-native) = "pa_deriving_params.cmo"
    archive(syntax,preprocessor,native) = "pa_deriving_params.cmx"

  package "link" (
  requires = "fmt,camlp5,pa_ppx_deriving.link,pa_ppx_base.link,camlp5.parser_quotations.link,camlp5.pa_r.link"
  archive(byte) = "pa_deriving_params.cmo"
  archive(native) = "pa_deriving_params.cmx"
  )
  requires = "fmt,pa_ppx_runtime,pa_ppx_params_runtime"
)
|} version)
