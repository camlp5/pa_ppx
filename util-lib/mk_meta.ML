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

Fmt.(pf stdout {|# Base libraries for "pa_ppx"
requires = "pcre,unix,camlp5.streams"
version = "%s"
description = "pa_ppx base libraries"

archive(byte) = "pa_ppx_utils.cma"
archive(native) = "pa_ppx_utils.cmxa"
|} version)
