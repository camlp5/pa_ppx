(**pp -syntax camlp5r *)
(* camlp5r *)
(* pa_deriving_fail.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;
open Pa_ppx_deriving ;
open Surveil ;
open Pa_deriving_base ;


value str_item_gen_fail name arg stri =
  Fmt.(raise_failwithf (loc_of_str_item stri) "Deriving.fail: failure at str_item")
;

value sig_item_gen_fail name arg sigi =
  Fmt.(raise_failwithf (loc_of_sig_item sigi) "Deriving.fail: failure at sig_item")
;

Pa_deriving.(Registry.add PI.{
  name = "fail"
; alternates = []
; options = ["optional"]
; default_options = let loc = Ploc.dummy in [ ("optional", <:expr< False >>) ]
; alg_attributes = []
; expr_extensions = []
; ctyp_extensions = []
; expr = (fun _ _ -> assert False)
; ctyp = (fun _ _ -> assert False)
; str_item = str_item_gen_fail
; sig_item = sig_item_gen_fail
})
;

