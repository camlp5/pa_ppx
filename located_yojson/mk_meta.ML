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

Fmt.(pf stdout {|# Json with locations (to help debug json deriver input errors)"
requires = "yojson,pa_ppx_base"
version = "%s"
description = "Json with locations"

archive(byte) = "pa_ppx_located_yojson.cma"
archive(native) = "pa_ppx_located_yojson.cmxa"
|} version)
