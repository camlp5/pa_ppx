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

Fmt.(pf stdout {|# Sexp with locations (to help debug sexp deriver input errors)"
requires = "sexplib"
version = "%s"
description = "Sexp with locations"

archive(byte) = "pa_ppx_located_sexp.cma"
archive(native) = "pa_ppx_located_sexp.cmxa"
|} version)
