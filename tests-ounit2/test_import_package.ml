(**pp -syntax camlp5o -package $(PAPACKAGES),pa_ppx_import,pa_ppx_located_yojson.pp,pa_ppx_located_sexp.pp *)

module LYJ = struct
[%%import: Pa_ppx_located_yojson.Json0.t
 [@with Ploc.t := Pa_ppx_base.Pp_MLast.Ploc.t]
]
[@@deriving show]
end

module LS = struct
[%%import: Pa_ppx_located_sexp.Sexp0.t
 [@with Ploc.t := Pa_ppx_base.Pp_MLast.Ploc.t]
]
[@@deriving show]
end
