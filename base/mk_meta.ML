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

Fmt.(pf stdout {|# Specifications for the "pa_ppx" preprocessor:
requires = "camlp5,rresult,fmt,pa_ppx_utils,pa_ppx_runtime,compiler-libs.common"
version = "%s"
description = "pa_ppx base support"

package "link" (
  requires = "pa_ppx_base"
)


archive(byte) = "pa_ppx_base.cma"
archive(native) = "pa_ppx_base.cmxa"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,rresult,fmt,pa_ppx_utils,pa_ppx_runtime,compiler-libs.common"
  archive(syntax,preprocessor,-native) = "pa_ppx_base.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_base.cmxa"
|} version)
