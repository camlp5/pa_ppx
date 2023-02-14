(** -syntax camlp5r *)
(* camlp5r *)
(* pa_undo_deriving.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_utils ;
open Pa_passthru ;
open Ppxutil ;

value flatten_implem arg (l, status) =
  let rec frec = fun [
    [ (<:str_item< declare $list:l$ end >>, _) :: t ] ->
    frec ((List.map (fun si -> (si, loc_of_str_item si)) l)@t)
  | [ h :: t ] -> [ h :: frec t ]
  | [] -> []
  ]
  in (frec l, status)
;

value flatten_interf arg (l, status) =
  let rec frec = fun [
    [ (<:sig_item< declare $list:l$ end >>, _) :: t ] ->
    frec ((List.map (fun si -> (si, loc_of_sig_item si)) l)@t)
  | [ h :: t ] -> [ h :: frec t ]
  | [] -> []
  ]
  in (frec l, status)
;

value flatten_signature arg l =
  let rec frec = fun [
    [ <:sig_item< declare $list:l$ end >> :: t ] ->
    frec (l@t)
  | [ h :: t ] -> [ h :: frec t ]
  | [] -> []
  ]
  in frec l
;

value flatten_structure arg l =
  let rec frec = fun [
    [ <:str_item< declare $list:l$ end >> :: t ] ->
    frec (l@t)
  | [ h :: t ] -> [ h :: frec t ]
  | [] -> []
  ]
  in frec l
;

value install () =
let ef = EF.mk() in
let ef = EF.{ (ef) with
  implem = extfun ef.implem with [
    z ->
      fun arg fallback ->
        Some (z |> flatten_implem arg |> fallback arg)
  ] } in

let ef = EF.{ (ef) with
  interf = extfun ef.interf with [
    z ->
      fun arg fallback ->
        Some (z |> flatten_interf arg |> fallback arg)
  ] } in

let ef = EF.{ (ef) with
  signature = extfun ef.signature with [
    z ->
      fun arg fallback ->
        Some (z |> flatten_signature arg |> fallback arg)
  ] } in

let ef = EF.{ (ef) with
  structure = extfun ef.structure with [
    z ->
      fun arg fallback ->
        Some (z |> flatten_structure arg |> fallback arg)
  ] } in

Pa_passthru.(install { name = "pa_normalize" ; ef = ef ; pass = Some 99 ; before = [] ; after = [] })
;

install();
