(**pp -syntax camlp5r *)
(* camlp5r *)
(* pa_ppx_runtime.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

type json = Json.t ;

module Stdlib = Stdlib ;

module Fmt = Fmt ;

value rec map_bind f acc xs =
  let open Rresult.R in 
  match xs with [
    [x :: xs] ->  (f x) >>= fun x -> map_bind f [x :: acc] xs
  | [] -> Result.Ok (List.rev acc) ]
;

value safe_map f l = List.rev (List.rev_map f l) ;

value result_to_located_yojson pa pb = fun [
  Ok arg0 -> (Ploc.dummy, `List [(Ploc.dummy, `String "Ok"); pa arg0])
| Error arg0 -> (Ploc.dummy, `List [(Ploc.dummy, `String "Error"); pb arg0])
]
;

open Rresult.R ;
module Yojson = struct
open Yojson ;
value result_of_located_yojson pa pb = fun [
  (_, `List [(_, `String "Ok"); arg0]) ->
  (pa arg0) >>= (fun arg0 -> Result.Ok (Ok arg0))
| (_, `List [(_, `String "Error"); arg0]) ->
  (pb arg0) >>= (fun arg0 -> Result.Ok (Error arg0))
| _ -> Result.Error "result"
]
;

(** These two copied and modified from Jane Street's sexplib0 *)
value hashtbl_to_located_yojson keymarsh valmarsh ht =
(Ploc.dummy, `List (Hashtbl.fold (fun k v acc -> [(Ploc.dummy, `List [keymarsh k; valmarsh v]) :: acc]) ht []))
;

value hashtbl_of_located_yojson keydemarsh valdemarsh (json : json) = match json with [
  (_, `List lst) ->
  let demarsh1 = fun [
    (_, `List[k;v]) ->
    (keydemarsh k) >>= (fun k -> (valdemarsh v) >>= (fun v -> Result.Ok (k,v)))
  | _ -> Result.Error "tuple list needed"
  ] in
  (List.fold_left (fun acc t ->
       acc >>= (fun acc -> (demarsh1 t) >>= (fun (k,v) -> Result.Ok [(k,v)::acc])))
      (Result.Ok[]) lst)
  >>= (fun l ->
      let ht = Hashtbl.create 5 in do {
        List.iter (fun (k,v) -> Hashtbl.add ht k v) l ;
        Result.Ok ht
      })
| _ -> Result.Error "list needed" ]
;

value unit_to_located_yojson = fun () -> (Ploc.dummy, `Null) ;
value int_to_located_yojson = fun x -> (Ploc.dummy, `Int x) ;
value bool_to_located_yojson = fun x -> (Ploc.dummy, `Bool x) ;
value int32_to_located_yojson = fun x -> (Ploc.dummy, `Intlit (Int32.to_string x)) ;
value int64_to_located_yojson = fun x -> (Ploc.dummy, `Intlit (Int64.to_string x)) ;
value string_to_located_yojson = fun x -> (Ploc.dummy, `String x) ;
value nativeint_to_located_yojson = fun x -> (Ploc.dummy, `Intlit (Nativeint.to_string x)) ;
value float_to_located_yojson = fun x -> (Ploc.dummy, `Float x) ;
value list_to_located_yojson f = fun x -> (Ploc.dummy, `List (safe_map f x)) ;
value array_to_located_yojson f = fun x -> (Ploc.dummy, `List (Array.to_list (Array.map f x))) ;
value ref_to_located_yojson f = fun x -> f x.val ;
value option_to_located_yojson f = fun [ None -> (Ploc.dummy, `Null) | Some x -> f x ] ;

value unit_of_located_yojson msg = fun [ (_, `Null) -> Result.Ok () | _ -> Result.Error msg ] ;
value int_of_located_yojson msg = fun [ (_, `Int x) -> Result.Ok x | _ -> Result.Error msg ] ;
value bool_of_located_yojson msg = fun [ (_, `Bool x) -> Result.Ok x | _ -> Result.Error msg ] ;
value int32_of_located_yojson msg = fun [
        (_, `Int x) -> Result.Ok (Int32.of_int x)
      | (_, `Intlit x) -> Result.Ok (Int32.of_string x)
      | _ -> Result.Error msg ] ;
value int64_of_located_yojson msg = fun [
      (_, `Int x) -> Result.Ok (Int64.of_int x)
      | (_, `Intlit x) -> Result.Ok (Int64.of_string x)
      | _ -> Result.Error msg ] ;
value string_of_located_yojson msg = fun [
        (_, `String x) -> Result.Ok x
      | _ -> Result.Error msg ] ;

value nativeint_of_located_yojson msg = fun [
        (_, `Int x) -> Result.Ok (Nativeint.of_int x)
      | (_, `Intlit x) -> Result.Ok (Nativeint.of_string x)
      | _ -> Result.Error msg ] ;
value float_of_located_yojson msg = fun [
        (_, `Int x) -> Result.Ok (float_of_int x)
      | (_, `Intlit x) -> Result.Ok (float_of_string x)
      | (_, `Float x) -> Result.Ok x
      | _ -> Result.Error msg ] ;
value list_of_located_yojson msg f = fun [
        (_, `List xs) -> map_bind f [] xs
      | _ -> Result.Error msg ] ;
value array_of_located_yojson msg f = fun [
        (_, `List xs) ->
          Rresult.R.bind (map_bind f [] xs)
             (fun x -> Result.Ok (Array.of_list x))
      | _ -> Result.Error msg ] ;
value ref_of_located_yojson f = fun x ->
  Rresult.R.bind (f x) (fun x -> Result.Ok (ref x)) ;
value option_of_located_yojson f = fun [
        (_, `Null) -> Result.Ok None
      | x ->
          Result.bind (f x) (fun x -> Result.Ok (Some x)) ] ;
end
;

