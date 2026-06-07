(**pp -syntax camlp5o -package pa_ppx_fail *)

let long_variable_name = 1 ;; [%fail "bar"] ;;
