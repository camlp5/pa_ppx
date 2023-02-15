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

Fmt.(pf stdout {|# Specifications for the "pa_ppx_undo_deriving" preprocessor:
requires = "pa_ppx_base,fmt,bos,compiler-libs.common"
version = "%s"
description = "pa_ppx_undo_deriving base support"

# For linking
package "link" (
requires = "pa_ppx_base.link,fmt,bos,compiler-libs.common"
archive(byte) = "pa_undo_deriving.cma"
archive(native) = "pa_undo_deriving.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_undo_deriving.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "pa_ppx_base,fmt,bos,compiler-libs.common"
  archive(syntax,preprocessor,-native) = "pa_undo_deriving.cma"
  archive(syntax,preprocessor,native) = "pa_undo_deriving.cmxa"
|} version)
