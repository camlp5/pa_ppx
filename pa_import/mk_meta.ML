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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_import" preprocessor:
requires = "camlp5.pa_o,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
version = "%s"
description = "pa_ppx_import base support"

# For linking
package "link" (
requires = "camlp5.pa_o.link,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
archive(byte) = "pa_import.cma"
archive(native) = "pa_import.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_import.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5.pa_o,pa_ppx_base,fmt,bos,compiler-libs.common,findlib.internal"
  archive(syntax,preprocessor,-native) = "pa_import.cma"
  archive(syntax,preprocessor,native) = "pa_import.cmxa"
|} version)
