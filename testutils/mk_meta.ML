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

Fmt.(pf stdout {|# test utilities for "pa_ppx"
requires = "unix,camlp5,oUnit,fmt,pcre,rresult,compiler-libs.common,yojson,sexplib0"
version = "%s"
description = "pa_ppx test utilities"

archive(byte) = "pa_ppx_testutils.cma"
archive(native) = "pa_ppx_testutils.cmxa"
|} version)
