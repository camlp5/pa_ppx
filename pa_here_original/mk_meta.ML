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
requires = "camlp5,fmt,pa_ppx_base"
version = "%s"
description = "pa_ppx pa_here support"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx_base.link"
archive(byte) = "pa_ppx_here_original.cma"
archive(native) = "pa_ppx_here_original.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_here_original.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx_base"
  archive(syntax,preprocessor,-native) = "pa_ppx_here_original.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_here_original.cmxa"
|} version)
