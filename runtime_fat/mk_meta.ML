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
requires = "rresult,fmt,sexplib0"
version = "%s"
description = "pa_ppx runtime support"

# For linking
archive(byte) = "pa_ppx_runtime_fat.cma"
archive(native) = "pa_ppx_runtime_fat.cmxa"

# For the toploop:
archive(byte,toploop) = "pa_ppx_runtime_fat.cma"
|} version)
