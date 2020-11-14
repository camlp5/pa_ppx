(* camlp5r *)
(* pp_MLast.ml,v *)

IFDEF BOOTSTRAP THEN

module Ploc = struct
include Ploc


let pp0_loc ppf loc =
  let fname = Ploc.file_name loc in
  let line = Ploc.line_nb loc in
  let bp = Ploc.first_pos loc in
  let ep = Ploc.last_pos loc in
  let bol = Ploc.bol_pos loc in

  let bp = bp - bol + 1 in
  let ep = ep - bol + 1 in
  Fmt.(pf ppf "<%a:%d:%d-%d>" (quote string) fname line bp ep)

let pp1_loc ppf x = Fmt.(const string "<loc>" ppf ())

let pp_loc_verbose = ref false

let pp ppf x =
  if !pp_loc_verbose then
    pp0_loc ppf x
  else
    pp1_loc ppf x

let equal (x : t) y = x = y

type 'a vala = [%import: 'a Ploc.vala] [@@deriving show,eq]
end

type loc = [%import: MLast.loc] [@@deriving show]
type type_var = [%import: MLast.type_var] [@@deriving show]

[%%import: MLast.expr] [@@deriving show]

ELSE
let show_longid _ = "<longid>"
let show_longid_lident _ = "<longid_lident>"
let pp_longid_lident pps x = Fmt.(pf pps "<longid_lident>")
let show_ctyp _ = "<ctyp>"
let pp_ctyp pps x = Fmt.(pf pps "<ctyp>")
let show_expr _ = "<expr>"
let pp_expr pps x = Fmt.(pf pps "<expr>")
let show_patt _ = "<patt>"
let pp_patt pps x = Fmt.(pf pps "<patt>")
let pp_attribute pps x = Fmt.(pf pps "<attribute>")

END

let open Pp_debug.Pp_MLast in
ref_show_longid := show_longid ;
ref_show_longid_lident := show_longid_lident ;
ref_show_ctyp := show_ctyp ;
ref_show_expr := show_expr ;
ref_show_patt := show_patt ;
()
;;
