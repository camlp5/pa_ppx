module Ploc =
  struct
    include Ploc;
    value pp0_loc ppf loc =
      let fname = Ploc.file_name loc in
      let line = Ploc.line_nb loc in
      let bp = Ploc.first_pos loc in
      let ep = Ploc.last_pos loc in
      let bol = Ploc.bol_pos loc in
      let bp = bp - bol in
      let ep = ep - bol in
      Fmt.(pf ppf "<%a:%d:%d-%d>" (quote string) fname line bp ep)
    ;
    value pp1_loc ppf x = Fmt.(const string "<loc>" ppf ());
    value pp_loc_verbose = ref False;
    value pp ppf x =
      if pp_loc_verbose.val then pp0_loc ppf x else pp1_loc ppf x
    ;
    value equal (x : t) y = x = y;
    type unmade_loc_t =
      (string * int * int * int * int * int * int * string * string)[@@"deriving_inline" (show, sexp, yojson);]
    ;
    value rec unmade_loc_t_to_yojson : unmade_loc_t → Yojson.Safe.t =
      fun arg →
        (let open! Runtime in
         let open! Stdlib in
         fun (v0, v1, v2, v3, v4, v5, v6, v7, v8) →
           `List
             [Runtime.Yojson.string_to_yojson v0;
              Runtime.Yojson.int_to_yojson v1;
              Runtime.Yojson.int_to_yojson v2;
              Runtime.Yojson.int_to_yojson v3;
              Runtime.Yojson.int_to_yojson v4;
              Runtime.Yojson.int_to_yojson v5;
              Runtime.Yojson.int_to_yojson v6;
              Runtime.Yojson.string_to_yojson v7;
              Runtime.Yojson.string_to_yojson v8])
          arg
    and unmade_loc_t_of_yojson : Yojson.Safe.t → Rresult.result unmade_loc_t string =
      fun arg →
        (let open! Runtime in
         let open! Stdlib in
         fun
         [ `List [v0; v1; v2; v3; v4; v5; v6; v7; v8] →
             Rresult.R.bind
               (Runtime.Yojson.string_of_yojson "Exceptions.Ploc.unmade_loc_t"
                  v0)
               (fun v0 →
                  Rresult.R.bind
                    (Runtime.Yojson.int_of_yojson
                       "Exceptions.Ploc.unmade_loc_t" v1)
                    (fun v1 →
                       Rresult.R.bind
                         (Runtime.Yojson.int_of_yojson
                            "Exceptions.Ploc.unmade_loc_t" v2)
                         (fun v2 →
                            Rresult.R.bind
                              (Runtime.Yojson.int_of_yojson
                                 "Exceptions.Ploc.unmade_loc_t" v3)
                              (fun v3 →
                                 Rresult.R.bind
                                   (Runtime.Yojson.int_of_yojson
                                      "Exceptions.Ploc.unmade_loc_t" v4)
                                   (fun v4 →
                                      Rresult.R.bind
                                        (Runtime.Yojson.int_of_yojson
                                           "Exceptions.Ploc.unmade_loc_t" v5)
                                        (fun v5 →
                                           Rresult.R.bind
                                             (Runtime.Yojson.int_of_yojson
                                                "Exceptions.Ploc.unmade_loc_t"
                                                v6)
                                             (fun v6 →
                                                Rresult.R.bind
                                                  (Runtime.Yojson.
                                                   string_of_yojson
                                                     "Exceptions.Ploc.unmade_loc_t"
                                                     v7)
                                                  (fun v7 →
                                                     Rresult.R.bind
                                                       (Runtime.Yojson.
                                                        string_of_yojson
                                                          "Exceptions.Ploc.unmade_loc_t"
                                                          v8)
                                                       (fun v8 →
                                                          Result.Ok
                                                            (v0, v1, v2, v3,
                                                             v4, v5, v6, v7,
                                                             v8))))))))))
         | _ → Result.Error "Exceptions.Ploc.unmade_loc_t" ])
          arg
    ;
    value rec sexp_of_unmade_loc_t : unmade_loc_t → Sexplib0.Sexp.t =
      fun arg →
        (let open! Runtime in
         let open! Stdlib in
         fun (v0, v1, v2, v3, v4, v5, v6, v7, v8) →
           Sexplib0.Sexp.List
             [Sexplib0.Sexp_conv.sexp_of_string v0;
              Sexplib0.Sexp_conv.sexp_of_int v1;
              Sexplib0.Sexp_conv.sexp_of_int v2;
              Sexplib0.Sexp_conv.sexp_of_int v3;
              Sexplib0.Sexp_conv.sexp_of_int v4;
              Sexplib0.Sexp_conv.sexp_of_int v5;
              Sexplib0.Sexp_conv.sexp_of_int v6;
              Sexplib0.Sexp_conv.sexp_of_string v7;
              Sexplib0.Sexp_conv.sexp_of_string v8])
          arg[@@"ocaml.warning" "-39";] [@@"ocaml.warning" "-33";]
    and unmade_loc_t_of_sexp : Sexplib0.Sexp.t → unmade_loc_t =
      fun arg →
        (let open! Runtime in
         let open! Stdlib in
         fun
         [ Sexplib0.Sexp.List [v0; v1; v2; v3; v4; v5; v6; v7; v8] →
             (Sexplib0.Sexp_conv.string_of_sexp v0,
              Sexplib0.Sexp_conv.int_of_sexp v1,
              Sexplib0.Sexp_conv.int_of_sexp v2,
              Sexplib0.Sexp_conv.int_of_sexp v3,
              Sexplib0.Sexp_conv.int_of_sexp v4,
              Sexplib0.Sexp_conv.int_of_sexp v5,
              Sexplib0.Sexp_conv.int_of_sexp v6,
              Sexplib0.Sexp_conv.string_of_sexp v7,
              Sexplib0.Sexp_conv.string_of_sexp v8)
         | _ → failwith "wrong number of members in list" ])
          arg[@@"ocaml.warning" "-39";] [@@"ocaml.warning" "-33";]
    ;
    value rec pp_unmade_loc_t : Fmt.t unmade_loc_t =
      fun (ofmt : Format.formatter) arg →
        (fun (ofmt : Format.formatter) (v0, v1, v2, v3, v4, v5, v6, v7, v8) →
           let open Runtime.Fmt in
           pf ofmt "(@[%a,@ %a,@ %a,@ %a,@ %a,@ %a,@ %a,@ %a,@ %a@])"
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v1
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v2
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v3
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v4
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v5
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v6
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v7
             (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v8)
          ofmt arg[@@"ocaml.warning" "-39";] [@@"ocaml.warning" "-33";]
    and show_unmade_loc_t : unmade_loc_t → Stdlib.String.t =
      fun arg → Format.asprintf "%a" pp_unmade_loc_t arg[@@"ocaml.warning" "-39";] [@@"ocaml.warning" "-33";]
    ;
    [@@@"end"];
    value unmk_t (x : t) : unmade_loc_t =
      let open Ploc in
      (file_name x, line_nb x, bol_pos x, line_nb_last x, bol_pos_last x,
       first_pos x, last_pos x, comment x, comment_last x)
    ;
    value mk_t
        ((file_name, line_nb, bol_pos, line_nb_last, bol_pos_last, first_pos,
          last_pos, comment, comment_last) :
         unmade_loc_t) =
      let x =
        Ploc.make_loc file_name line_nb bol_pos (first_pos, last_pos) comment
      in
      let x = Ploc.with_comment_last x comment_last in
      let x = Ploc.with_line_nb_last x line_nb_last in
      let x = Ploc.with_bol_pos_last x bol_pos_last in
      x
    ;
    value to_yojson (x : t) = unmade_loc_t_to_yojson (unmk_t x);
    value of_yojson j =
      Rresult.R.bind (unmade_loc_t_of_yojson j) (fun x → Result.Ok (mk_t x))
    ;
    value sexp_of_t (x : t) = sexp_of_unmade_loc_t (unmk_t x);
    value t_of_sexp s = mk_t (unmade_loc_t_of_sexp s);
  end
;
type t = exn == ..[@@"deriving_inline" show;];
[@@@"ocaml.text" "/*";];
module M_pp =
  struct
    type nonrec pp = { f : mutable Fmt.t t };
    value f =
      {f _ =
        invalid_arg
          ("pp: Maybe a [@@deriving show] is missing when extending the type " ^
           "t")}
    ;
  end
;
[@@@"ocaml.text" "/*";];
value pp x = M_pp.f.M_pp.f x;
value show arg = Format.asprintf "%a" M_pp.f.M_pp.f arg;
[@@@"end"];
type t +=
  [ Help = Arg.Help[@"name" "Arg.Help";]
  | Bad = Arg.Bad[@"name" "Arg.Bad";]
  | Finally_raised = Fun.Finally_raised[@"name" "Fun.Finally_raised";]
  | Undefined = Lazy.Undefined[@"name" "Lazy.Undefined";]
  | Parse_error = Parsing.Parse_error[@"name" "Parsing.Parse_error";]
  | QueueEmpty = Queue.Empty[@"name" "Queue.Empty";]
  | Scan_failure = Scanf.Scan_failure[@"name" "Scanf.Scan_failure";]
  | StackEmpty = Stack.Empty[@"name" "Stack.Empty";]
  | Exit = Stdlib.Exit[@"name" "Stdlib.Exit";]
  | Match_failure = Stdlib.Match_failure[@"name" "Stdlib.Match_failure";]
  | Assert_failure = Stdlib.Assert_failure[@"name" "Stdlib.Assert_failure";]
  | Invalid_argument
    = Stdlib.Invalid_argument[@"name" "Stdlib.Invalid_argument";]
  | Failure = Stdlib.Failure[@"name" "Stdlib.Failure";]
  | Not_found = Stdlib.Not_found[@"name" "Stdlib.Not_found";]
  | Out_of_memory = Stdlib.Out_of_memory[@"name" "Stdlib.Out_of_memory";]
  | Stack_overflow = Stdlib.Stack_overflow[@"name" "Stdlib.Stack_overflow";]
  | Sys_error = Stdlib.Sys_error[@"name" "Stdlib.Sys_error";]
  | End_of_file = Stdlib.End_of_file[@"name" "Stdlib.End_of_file";]
  | Division_by_zero
    = Stdlib.Division_by_zero[@"name" "Stdlib.Division_by_zero";]
  | Sys_blocked_io = Stdlib.Sys_blocked_io[@"name" "Stdlib.Sys_blocked_io";]
  | Undefined_recursive_module
    = Stdlib.Undefined_recursive_module[@"name" "Stdlib.Undefined_recursive_module";]
  | StreamFailure = Stream.Failure[@"name" "Stream.Failure";]
  | Error = Stream.Error[@"name" "Stream.Error";]
  | Break = Sys.Break[@"name" "Sys.Break";]
  | Exc = Ploc.Exc[@"name" "Ploc.Exc";] ][@@"deriving_inline" show;]
;
let open M_pp in
let fallback = f.f in
f.f :=
  fun ofmt →
    fun
    [ Help v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Arg.Help@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Bad v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Arg.Bad@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Finally_raised v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Fun.Finally_raised@ %a)@]" pp v0
    | Undefined → let open Runtime.Fmt in pf ofmt "@[<2>Lazy.Undefined@]"
    | Parse_error →
        let open Runtime.Fmt in pf ofmt "@[<2>Parsing.Parse_error@]"
    | QueueEmpty → let open Runtime.Fmt in pf ofmt "@[<2>Queue.Empty@]"
    | Scan_failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Scanf.Scan_failure@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | StackEmpty → let open Runtime.Fmt in pf ofmt "@[<2>Stack.Empty@]"
    | Exit → let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Exit@]"
    | Match_failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Match_failure@ %a)@]"
          (fun (ofmt : Format.formatter) (v0, v1, v2) →
             let open Runtime.Fmt in
             pf ofmt "(@[%a,@ %a,@ %a@])"
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v1
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v2)
          v0
    | Assert_failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Assert_failure@ %a)@]"
          (fun (ofmt : Format.formatter) (v0, v1, v2) →
             let open Runtime.Fmt in
             pf ofmt "(@[%a,@ %a,@ %a@])"
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v1
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v2)
          v0
    | Invalid_argument v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Invalid_argument@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Failure v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Failure@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Not_found → let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Not_found@]"
    | Out_of_memory →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Out_of_memory@]"
    | Stack_overflow →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Stack_overflow@]"
    | Sys_error v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Sys_error@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | End_of_file →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.End_of_file@]"
    | Division_by_zero →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Division_by_zero@]"
    | Sys_blocked_io →
        let open Runtime.Fmt in pf ofmt "@[<2>Stdlib.Sys_blocked_io@]"
    | Undefined_recursive_module v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stdlib.Undefined_recursive_module@ %a)@]"
          (fun (ofmt : Format.formatter) (v0, v1, v2) →
             let open Runtime.Fmt in
             pf ofmt "(@[%a,@ %a,@ %a@])"
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v1
               (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%d" arg) v2)
          v0
    | StreamFailure → let open Runtime.Fmt in pf ofmt "@[<2>Stream.Failure@]"
    | Error v0 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Stream.Error@ %a)@]"
          (fun ofmt arg → let open Runtime.Fmt in pf ofmt "%S" arg) v0
    | Break → let open Runtime.Fmt in pf ofmt "@[<2>Sys.Break@]"
    | Exc v0 v1 →
        let open Runtime.Fmt in
        pf ofmt "(@[<2>Ploc.Exc@ (@,%a,@ %a@,))@]" Ploc.pp v0 pp v1
    | z → fallback ofmt z ];
[@@@"end"];
value print_exn exn = Some (show exn);
Printexc.register_printer print_exn;


