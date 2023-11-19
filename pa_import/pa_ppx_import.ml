(**pp -syntax camlp5r *)
(* pa_import.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Pa_passthru ;
open Ppxutil ;

value debug = Pa_passthru.debug ;

value mli_only = ref False ;
value redeclare = ref False ;
value predicates = ref [] ;
value lookup_path =
  let rec loop acc = fun [
      [] | ["--" :: _] -> acc
      | ["-I" ; dir :: tl ] -> loop [dir :: acc] tl
      | [_ :: tl] -> loop acc tl
      ] in ref ["." :: (loop [] (Array.to_list Sys.argv))]
;

value reset_switches () = do {
  mli_only.val := False
; redeclare.val := False
; predicates.val := []
; lookup_path.val := []
}
;

value add_predicates s =
  let l = String.split_on_char ',' s in
  let l = Std.filter (fun s -> s <> "") l in
  List.iter (Std.push predicates) l
;

value add_package s =
  let l = String.split_on_char ',' s in
  let l = Std.filter (fun s -> s <> "") l in
  let pl = Findlib.package_deep_ancestors predicates.val l in
  List.iter (fun p ->
      let d = Findlib.package_directory p in
      Std.push lookup_path d) pl
;

value add_include s =
  Std.push lookup_path (Findlib.resolve_path s)
;

value pp_report pps () =
  let path = lookup_path.val in
  let path = String.concat ":" path in
  Fmt.(pf pps "[import_type: packages: %s]\n%!" path)
;

value report () = pp_report Fmt.stderr () ;

value lookup1 fname d =
  let f = Fpath.add_seg (Fpath.v d) fname in
  let r = Bos.OS.File.exists f in
  if Rresult.R.is_ok r then
    if Rresult.R.get_ok r then
      f
    else failwith "caught"
  else failwith "caught"
;


value lookup_file suff fmod =
  let fname = Printf.sprintf "%s.%s" (String.uncapitalize_ascii fmod) suff in
  match try_find (fun s -> lookup1 fname s) lookup_path.val with [
    f -> Fmt.(str "%a" Fpath.pp f)
  | exception Failure _ -> Fmt.(failwithf "lookup_file: module %s (%s) not found\n%a"
                                  fmod suff
                                  pp_report ()
                           )
  ]
;


module CMI = struct
value _demarsh loc infile =
    let x = Cmi_format.read_cmi infile in
    x.Cmi_format.cmi_sign
;

value cache = ref [] ;
value demarsh loc infile =
  match List.assoc infile cache.val with [
    x -> x
  | exception Not_found ->
    let rv = _demarsh loc infile in do {
      Std.push cache (infile,rv) ;
      rv
    }
  ]
;

value extract_module_from_module_declaration md =
  let open Types in
  match md with [
      {md_type=Mty_signature l} -> Some l
    | _ -> None
    ]
;

value extract_module_from_modtype_declaration md =
  let open Types in
  match md with [
      {mtd_type=Some (Mty_signature l)} -> Some l
    | _ -> None
    ]
;

value extract_module ~{impl_only} mname si =
  let open Types in
  match si with [
      Sig_module id _ md _ _ when Ident.name id = mname -> extract_module_from_module_declaration md
    | Sig_modtype id mtd _ when not impl_only && Ident.name id = mname -> extract_module_from_modtype_declaration mtd
    | _ -> None
    ]
;

value rec lookup_signature ~{impl_only} li =
  match li with [
      <:longident:< $uid:fmod$ >> ->
        let infile = lookup_file "cmi" fmod in
        demarsh loc infile
    | <:longident:< $longid:li$ . $uid:mname$ >> ->
        let sil = lookup_signature ~{impl_only=impl_only} li in
        match List.find_map (extract_module ~{impl_only=impl_only} mname) sil with [
            Some l -> l
          | None -> Fmt.(raise_failwithf loc "pa_import: cannot find module %s (cmi)" mname)
          ]
    ]
;

value reparse_signature li sil =
    let txt = Fmt.(str "%a%!" Printtyp.signature sil) in
    try
      List.map fst (fst (Pcaml.parse_interf (Stream.of_string txt)))
    with exc -> do {
      let rbt = Printexc.get_raw_backtrace() in
      Fmt.(pf stderr "ERROR: reparse_signature %a: exception raised while reparsing CMI text:\n<<%s>>\n: %a"
             Pp_MLast.pp_longid li
             txt
             exn exc);
      Printexc.raise_with_backtrace exc rbt
    }
;

value load_signature ~{impl_only} li =
  let sil = lookup_signature ~{impl_only=impl_only} li in
  reparse_signature li sil
;

end ;

module MLI = struct

value parse_mli infile =
  let txt = infile |> Fpath.v|> Bos.OS.File.read |> Rresult.R.get_ok in
  List.map fst (fst (Pcaml.parse_interf (Stream.of_string txt)))
;

value _demarsh loc infile =
  parse_mli infile
;

value cache = ref [] ;
value demarsh loc infile =
  match List.assoc infile cache.val with [
    x -> x
  | exception Not_found ->
    let rv = _demarsh loc infile in do {
      Std.push cache (infile,rv) ;
      rv
    }
  ]
;

value extract_module1 ~{impl_only} mname si =
  match si with [
      <:sig_item< module $uid:id$ : sig $list:sil$ end >> when mname = id ->
        Some sil
    | <:sig_item< module type $id$ = sig $list:sil$ end >> when not impl_only && mname = id ->
        Some sil
    | _ -> None
    ]
;

value extract_module ~{impl_only} mname sil =
  match List.find_map (extract_module1 ~{impl_only=impl_only} mname) sil with [
      Some l -> l
    | None -> Fmt.(raise_failwithf (MLast.loc_of_sig_item (List.hd sil)) "pa_import: cannot find module %s (mli)" mname)
    ]
;

value rec load_signature ~{impl_only} li =
  match li with [
      <:longident:< $uid:fmod$ >> ->
        let infile = lookup_file "mli" fmod in
        demarsh loc infile
    | <:longident:< $longid:li$ . $uid:mname$ >> ->
        let sil = load_signature ~{impl_only=impl_only} li in
        extract_module ~{impl_only=impl_only} mname sil
    ]
;

end ;

module Lookup = struct

value lookup_signature ~{impl_only=impl_only} li =
  try
    CMI.load_signature ~{impl_only=impl_only} li
  with exc1 ->
    try
      MLI.load_signature ~{impl_only=impl_only} li
    with exc2 ->
      Fmt.(raise_failwithf (MLast.loc_of_longid li) "pa_import: lookup_signature (impl_only=%a): cannot find module %a\n%a\n%a"
             bool impl_only
             Pp_MLast.pp_longid li
             exn exc1
             exn exc2)
;

value extract_typedecl lid = fun [
  <:sig_item:< type $flag:nrfl$ $list:tdl$ >> ->
   List.find_map (fun td -> if uv (snd (uv td.tdNam)) = lid then Some (td,(nrfl, tdl)) else None) tdl
| _ -> None
]
;

value lookup_typedecl li lid =
  List.find_map (extract_typedecl lid) (lookup_signature ~{impl_only=False} li)
;

value import_typedecl t = do {
  if debug.val then report() else () ;
  match fst (Ctyp.unapplist t)  with [
    <:ctyp< $lid:lid$ >> -> failwith "CMI.import_typedecl: self-type-lookup not implemented"
  | <:ctyp:< $longid:modname$ . $lid:lid$ >> ->
    match lookup_typedecl modname lid with [
        Some x -> x
      | None -> Fmt.(raise_failwithf loc "CMI.import_typedecl: typedecl %a not found" Pp_MLast.pp_ctyp t)
      ]
  ]
}
;

value rec import_module_type arg t =
  match t with [
    <:ctyp< $t$ [@ $attribute:attr$ ] >> ->
      import_module_type arg t
  | <:ctyp:< ( module  $longid:li$ . $lid:i$ ) >> ->
     let sil = lookup_signature ~{impl_only=False} li in
     let sil = MLI.extract_module ~{impl_only=False} i sil in
     <:module_type< sig $list:sil$ end >>
  | <:ctyp:< ( module  $longid:li$ ) >> ->
     let sil = lookup_signature ~{impl_only=False} li in
     <:module_type< sig $list:sil$ end >>
  ]
;

end ;

value logged_parse f fname =
  let st = Unix.gettimeofday () in
  let rv = f fname in
  let et = Unix.gettimeofday () in do {
    Fmt.(pf stderr "[parse %s in %f secs]\n%!" fname (et -. st)) ;
    rv
  }
;

module RM = struct
  value (=) = Reloc.eq_ctyp ;
  value rec assoc x = fun [
    [] -> raise Not_found
  | [(a,b)::l] -> if  a = x then b else assoc x l ];

value rec mem_assoc x = fun [
    [] -> False
  | [(a, _) :: l] ->  a = x || mem_assoc x l ];

end
;

value substitute_ctyp renmap t =
  let rec subrec t =
    if RM.mem_assoc t renmap then
      RM.assoc t renmap
    else match t with [
    <:ctyp:< ( $list:l$ ) >> -> <:ctyp< ( $list:List.map subrec l$ ) >>
  | <:ctyp:< { $list:ldl$ } >> ->
    let sub_label_decl (loc, na,b,ty,al) =
      (loc,na,b,subrec ty, al) in
    <:ctyp< { $list:List.map sub_label_decl ldl$ } >>
  | <:ctyp:< [ $list:l$ ] >> ->
    let l = List.map (fun (loc, cid, tyvars, tyl, tyo, attrs) ->
        (loc, cid, tyvars, Pcaml.vala_map (List.map subrec) tyl, vala_map (option_map subrec) tyo, attrs)
      ) l in
      <:ctyp< [ $list:l$ ] >>
  | <:ctyp:< $_$ $_$ >> as z ->
    subst1 (Ctyp.unapplist z)
  | z -> subst1 (z,[])
  ]
  and subst1 (t,args) =
    let args = List.map subrec args in
    if not (RM.mem_assoc t renmap) then Ctyp.applist t args
    else match RM.assoc t renmap with [
      <:ctyp:< $t$ [@ "polyprinter" $e$ ; ] >> ->
        <:ctyp< $Ctyp.applist t args$ [@ "polyprinter" $e$ ; ] >>
    | t -> Ctyp.applist t args
    ]
  in subrec t
;

value expr_to_ctyp0 = fun [
  <:expr:< $longid:li$ . $lid:id$ >> -> <:ctyp< $longid:li$ . $lid:id$ >>
| <:expr:< $lid:id$ >> -> <:ctyp< $lid:id$ >>
| <:expr< [%typ: $type:ty$] >> -> ty
]
;

value expr_to_ctyp loc e =
  match e with [
    <:expr:< $e$ [@ $attribute:a$ ] >> ->
    let ct = expr_to_ctyp0 e in
    <:ctyp:< $ct$ [@ $attribute:a$ ] >>
  | e -> expr_to_ctyp0 e
  ]
;

value assignment_to_subst = fun [
  <:expr:< $e1$ . val := $e2$ >> ->
    let t1 = expr_to_ctyp loc e1 in
    let t2 = expr_to_ctyp loc e2 in
    if Reloc.eq_ctyp t1 t2 then [] else [(t1, t2)]
]
;

value extend_renmap attr renmap =
  let e = match uv attr with [
    <:attribute_body<"with" $exp:e$ ; >> -> e
  | _ -> failwith "import: unrecognized attribute"
  ] in
  let l = match e with [
    <:expr< $_$ := $_$ >> -> assignment_to_subst e
  | <:expr< do { $list:l$ } >> ->
    List.concat (List.map assignment_to_subst l)
  ] in
  l @ renmap
;

value extract_with_attributes attrs =
  filter_split (fun a -> "with" = (attr_id a)) attrs
;

value extract_synonym_attributes attrs =
  filter_split (fun a -> "synonym" = (attr_id a)) attrs
;

value extract_add_attributes attrs =
  filter_split (fun a -> "add" = (attr_id a)) attrs
;

type unpacked_t =
  {
    full_t : ctyp
  ; attrs : list attribute
  ; bare_t : ctyp
  ; unapp_t : ctyp
  ; args : list ctyp
  ; li : longid
  ; lid : string
  ; sl : list string
  ; loc : Ploc.t
  ; self_import : bool
  }
;

value unpack_imported_type arg full_t =
  let (bare_t,attrs) = Ctyp.unwrap_attrs full_t in
  let (unapp_t, args) = Ctyp.unapplist bare_t in
  let (li, lid) = match unapp_t with [
    <:ctyp< $longid:li$ . $lid:lid$ >> -> (li, lid)
  | _ -> failwith "unpack_imported_type"
  ] in
  let sl = Longid.to_string_list li in
  let self_import = (List.hd sl) = (List.hd (Ctxt.module_path arg)) in
  { full_t = full_t ; attrs = attrs ; bare_t = bare_t ;
    unapp_t = unapp_t ; args = args ; li = li ;
    lid = lid ; sl = sl ; loc = loc_of_ctyp full_t
  ; self_import = self_import }
;

value import_type arg (newtname,new_formals) t =
  let unp = unpack_imported_type arg t in
  let loc = unp.loc in
  let (with_attrs, rest_attrs) = extract_with_attributes unp.attrs in
  let (synonym_attrs,_) = extract_synonym_attributes unp.attrs in
  let synonym_attr = match synonym_attrs with [
        [a] when redeclare.val -> Some (uv a)
      | [] -> None
      | _ ->
         Fmt.(raise_failwithf loc "pa_import: can only provide at most one synonym attribute and ONLY when -pa_import-redeclare is supplied on the commandline")
      ] in
  let unp = { (unp) with attrs = rest_attrs } in
  let renmap = List.fold_right extend_renmap with_attrs [] in
  let actuals = unp.args in
  let (td, tdl) = Lookup.import_typedecl unp.unapp_t in
  let formals = uv td.tdPrm in
    if List.length formals <> List.length actuals then
      failwith "import_type: type-param formal/actual list-length mismatch"
    else let renmap = List.fold_left2 (fun renmap f a ->
        match (uv (fst f), a) with [
          (None, _) -> renmap
        | (Some fid, <:ctyp< ' $id$ >>) when fid <> id ->
          let fid = <:ctyp< ' $fid$ >> in
          [ (fid, a) :: renmap ]
        | _ -> renmap
        ]) renmap formals actuals in
    let tk = match td.tdDef with [
      <:ctyp< $_$ == $t$ >> -> t
    | t -> t
    ] in
    let ct = if renmap = [] then tk
    else Ctyp.wrap_attrs (substitute_ctyp renmap tk) unp.attrs in
    if is_generative_type ct && not redeclare.val && not unp.self_import then
      <:ctyp< $unp.bare_t$ == $ct$ >>
    else if redeclare.val then
      let ct = match ct with [
            <:ctyp< $_$ == $ct$ >> -> ct
          | _ -> ct
          ] in
      match synonym_attr with [
          Some <:attribute_body<"synonym": $type:ty$ >> -> <:ctyp< $ty$ == $ct$ >>
        | None -> ct
        ]
    else ct
;

value rec expand_add_attribute arg attr =
  let sil = match uv attr with [
    <:attribute_body< "add" $structure:l$ >> when l <> [] -> l
  | _ -> Ploc.raise (attr |> uv |> fst |> uv |> fst)
      (Failure Fmt.(str "expand_add_attribute: malformed add attribute:@ %a"
                      Pp_MLast.pp_attribute attr))
  ] in
  let expanded_sil = sil |> List.map (fun [
    <:str_item:<  type $flag:_$ $list:_$ >>
  | <:str_item:< [%% import: $type:_$ ] $itemattrs:_$ >> as si ->
      registered_str_item_extension arg si

  ]) in
  expanded_sil |> List.concat_map (fun [
    <:str_item< type $flag:_$ $list:l$ >> -> l
  | _ -> assert False
  ])

and import_typedecl_group arg t item_attrs =
  let unp = unpack_imported_type arg t in
  let (with_attrs, rest_attrs) = extract_with_attributes unp.attrs in
  let (add_attrs, rest_attrs) = extract_add_attributes rest_attrs in
  let added_typedecls = List.concat (List.map (expand_add_attribute arg) add_attrs) in
  let unp = { (unp) with attrs = rest_attrs } in
  let renmap = List.fold_right extend_renmap with_attrs [] in
  let loc = unp.loc in
  let (rd, (nrfl, tdl)) = Lookup.import_typedecl unp.unapp_t in
  let tdl = tdl @ added_typedecls in
  let tdl = List.map (fun td ->
      let imported_tycon =
        let tyna = uv (snd (uv td.tdNam)) in
        let baset = <:ctyp< $longid:unp.li$ . $lid:tyna$ >> in
        let type_var2arg (so, _) =
          match uv so with [
            Some s -> <:ctyp< ' $s$ >>
          | None ->
            failwith "import_typedecl_group: cannot import typedecl group where some params are unnamed"
          ] in
        let args = List.map type_var2arg (uv td.tdPrm) in
        Ctyp.applist baset args in
      let ct = if renmap = [] then td.tdDef
        else Ctyp.wrap_attrs (substitute_ctyp renmap td.tdDef) unp.attrs in
      let ct = if is_generative_type ct && not redeclare.val && not unp.self_import then
          <:ctyp< $imported_tycon$ == $ct$ >>
               else if redeclare.val then
                 match ct with [
                     <:ctyp< $_$ == $ct$ >> -> ct
                   | _ -> ct
                   ]
               else ct in
      { (td) with tdDef = ct }
    ) tdl in
  let (last, tdl) = sep_last tdl in
  let last =
    let last_attrs = uv last.tdAttributes in
    let new_attrs = last_attrs@item_attrs in
    { (last) with tdAttributes = <:vala< new_attrs >> } in
  let tdl = tdl @ [last] in
  (nrfl, tdl)

and registered_str_item_extension arg = fun [
  <:str_item:< type $flag:nrfl$ $list:tdl$ >> ->
    let tdl = List.map (fun td ->
        match td.tdDef with [
          <:ctyp< [% import: $type:t$ ] >> ->
          let tname = uv td.tdNam in
          let l = uv td.tdPrm in
          let ct = import_type arg (tname,l) t in
          { (td) with tdDef = ct }
        | _ -> td
        ]
      ) tdl in
    <:str_item< type $flag:nrfl$ $list:tdl$ >>

  | <:str_item:< [%% import: $type:t$ ] $itemattrs:item_attrs$ >> ->
    let (nrfl, tdl) = import_typedecl_group arg t item_attrs in
    <:str_item< type $flag:nrfl$ $list:tdl$ >>
| _ -> assert False
]
;

value registered_sig_item_extension arg = fun [
  <:sig_item:< type $flag:nrfl$ $list:tdl$ >> ->
    let tdl = List.map (fun td ->
        match td.tdDef with [
          <:ctyp< [% import: $type:t$ ] >> ->
          let tname = uv td.tdNam in
          let l = uv td.tdPrm in
          let ct = import_type arg (tname,l) t in
          { (td) with tdDef = ct }
        | _ -> td
        ]
      ) tdl in
    <:sig_item< type $flag:nrfl$ $list:tdl$ >>

  | <:sig_item:< [%% import: $type:t$ ] $itemattrs:item_attrs$ >> ->
    let (nrfl, tdl) = import_typedecl_group arg t item_attrs in
    <:sig_item< type $flag:nrfl$ $list:tdl$ >>
| _ -> assert False
]
;

value registered_module_type_extension arg = fun [
  <:module_type:< [% import: $type:t$ ] >> ->
    Some (Lookup.import_module_type arg t)
| _ -> assert False
]
;

value install () =
let ef = EF.mk() in
let ef = EF.{ (ef) with
  str_item = extfun ef.str_item with [
    <:str_item:< type $flag:_$ $list:_$ >> as z ->
      fun arg _ ->
        Some (registered_str_item_extension arg z)

  | <:str_item< [%% import: $type:_$ ] $itemattrs:_$ >> as z -> 
      fun arg _ ->
        Some (registered_str_item_extension arg z)

  ] } in

let ef = EF.{ (ef) with
  sig_item = extfun ef.sig_item with [
    <:sig_item:< type $flag:_$ $list:_$ >> as z ->
      fun arg _ ->
        Some (registered_sig_item_extension arg z)

  | <:sig_item< [%% import: $type:_$ ] $itemattrs:_$ >> as z -> 
      fun arg _ ->
        Some (registered_sig_item_extension arg z)

  ] } in

let ef = EF.{ (ef) with
  module_type = extfun ef.module_type with [
    <:module_type:< [% import: $type:_$ ] >> as z ->
      fun arg _ ->
        registered_module_type_extension arg z
  ] } in
Pa_passthru.(install { name = "pa_import" ; ef = ef ; pass = None ; before = [] ; after = [] })
;

Pcaml.add_option "-pa_import-package" (Arg.String add_package)
  "<string> list of packages to search for CMI files.";

Pcaml.add_option "-pa_import-predicates" (Arg.String add_predicates)
  "<string> list of findlib predicates to add when searching CMI files.";

Pcaml.add_option "-pa_import-I" (Arg.String add_include)
  "<string> include-directory to search for CMI files.";

Pcaml.add_option "-pa_import-mli-only" (Arg.Set mli_only)
  "<string> use only MLI (not CMI) files.";

Pcaml.add_option "-pa_import-redeclare" (Arg.Set redeclare)
  "<string> redeclare types (do not re-export) -- useful for using types from other Ocaml versions.";

Pcaml.add_option "-pa_import-reset" (Arg.Unit reset_switches)
  "reset and clear all flags for pa_import.";

(* calls lazy_init() so we're sure of being inited *)
add_include (Findlib.ocaml_stdlib());

install();
