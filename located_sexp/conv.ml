(**pp -syntax camlp5r -package sexplib *)

open Sexp ;

value apply_converter converter se =
  try converter (to_sexplib_sexp se)
  with exn ->
    let loc = loc_of_sexp se in
    raise (Ploc.Exc loc exn)
;

(* copied from sexplib0/sexp_conv.ml *)
value hashtbl_of_sexp key_of_sexp val_of_sexp sexp =
  match sexp with [
      List _ lst ->
    let htbl = Hashtbl.create 0 in
    let act = fun [
       List _ [ k_sexp; v_sexp ] ->
        Hashtbl.add htbl (key_of_sexp k_sexp) (val_of_sexp v_sexp)
      | (List _ _ | Atom _ _) as se ->
         raise (Ploc.Exc (loc_of_sexp se) (Sexplib0.Sexp.Of_sexp_error (Failure "hashtbl_of_sexp: tuple list needed") (to_sexplib_sexp se)))
        ] in do {
        List.iter act lst;
        htbl
      }
  | (Atom loc _) as se ->
     raise (Ploc.Exc (loc_of_sexp se) (Sexplib0.Sexp.Of_sexp_error (Failure "hashtbl_of_sexp: list needed") (to_sexplib_sexp se)))
    ]
;

value list_of_sexp elemf se =
  match se with [
      Atom loc _ -> raise (Ploc.Exc (loc_of_sexp se) (Sexplib0.Sexp.Of_sexp_error (Failure "list_of_sexp: list needed") (to_sexplib_sexp se)))
    | List _ l -> List.map elemf l
    ]
;
value sexp_of_list loc elemf l =
  List loc (List.map elemf l)
;

value array_of_sexp elemf se =
  match se with [
      Atom loc _ -> raise (Ploc.Exc (loc_of_sexp se) (Sexplib0.Sexp.Of_sexp_error (Failure "array_of_sexp: list needed") (to_sexplib_sexp se)))
    | List _ [] -> [||]
    | List _ [h::t] ->
       let a = Array.make (1 + List.length t) (elemf h) in do {
        List.iteri (fun i se -> Array.set a (i+1) (elemf se)) t ;
        a
      }
    ]
;
value sexp_of_array loc elemf a =
  let l = Array.to_list a in
  sexp_of_list loc elemf l
;

value ref_of_sexp elemf se = ref (elemf se) ;
value sexp_of_ref loc elemf v = elemf v.val ;

value option_of_sexp elemf se =
  match se with [
      Atom _ _ -> raise (Ploc.Exc (loc_of_sexp se) (Sexplib0.Sexp.Of_sexp_error (Failure "option_of_sexp: list needed") (to_sexplib_sexp se)))
    | List _ [] -> None
    | List _ [v] -> Some (elemf v)
    | _ -> raise (Ploc.Exc (loc_of_sexp se) (Sexplib0.Sexp.Of_sexp_error (Failure "option_of_sexp: length <= 1 list needed") (to_sexplib_sexp se)))
    ]
;
value sexp_of_option loc elemf = fun [
  None -> List loc []
| Some v -> List loc [elemf v]
]
;

value unit_of_sexp =  apply_converter Sexplib0.Sexp_conv.unit_of_sexp ;
value sexp_of_unit loc x =  Sexp.of_sexplib_sexp loc (Sexplib0.Sexp_conv.sexp_of_unit x) ;

value int_of_sexp =  apply_converter Sexplib0.Sexp_conv.int_of_sexp ;
value sexp_of_int loc x =  Sexp.of_sexplib_sexp loc (Sexplib0.Sexp_conv.sexp_of_int x) ;

value string_of_sexp =  apply_converter Sexplib0.Sexp_conv.string_of_sexp ;
value sexp_of_string loc x =  Sexp.of_sexplib_sexp loc (Sexplib0.Sexp_conv.sexp_of_string x) ;

value float_of_sexp =  apply_converter Sexplib0.Sexp_conv.float_of_sexp ;
value sexp_of_float loc x =  Sexp.of_sexplib_sexp loc (Sexplib0.Sexp_conv.sexp_of_float x) ;
