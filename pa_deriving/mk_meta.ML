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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_deriving" preprocessor:
version = "%s"
description = "pa_ppx_deriving base support"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx_base.link"
archive(byte) = "pa_deriving.cma"
archive(native) = "pa_deriving.cmxa"
)
requires = "camlp5,fmt,pa_ppx_runtime"

# For the toploop:
requires(byte,toploop) = "camlp5,fmt,pa_ppx_base,pa_ppx_runtime"
archive(byte,toploop) = "pa_deriving.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor,-native) = "pa_deriving.cma"
  archive(syntax,preprocessor,native) = "pa_deriving.cmxa"
|} version)
