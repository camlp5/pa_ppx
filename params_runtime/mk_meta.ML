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
requires = "rresult,fmt,pa_ppx_deriving,pa_ppx_runtime,uint,pa_ppx_base"
version = "%s"
description = "pa_ppx params runtime support"

# For linking
archive(byte) = "pa_ppx_params.cma"
archive(native) = "pa_ppx_params.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_params.cma"
|} version)
