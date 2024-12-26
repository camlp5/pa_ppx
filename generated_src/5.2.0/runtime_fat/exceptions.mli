(**pp -syntax camlp5r *)
module Ploc :
  sig
    include module type of Ploc with
      type t = Ploc.t
      and type vala α = Ploc.vala α;
    value pp_loc_verbose : ref bool;
    value pp : Fmt.t t;
    value equal : t → t → bool;
  end
;
type t = exn == ..[@@"deriving_inline" (show, sexp, yojson, eq);];
[@@@"ocaml.text" "/*";];
module M_equal :
  sig
    type nonrec equal = { f : mutable t → t → Stdlib.Bool.t };
    value f : equal;
  end
;
[@@@"ocaml.text" "/*";];
value equal : t → t → Stdlib.Bool.t;
[@@@"ocaml.text" "/*";];
module M_to_yojson :
  sig
    type nonrec to_yojson = { f : mutable t → Yojson.Safe.t };
    value f : to_yojson;
  end
;
[@@@"ocaml.text" "/*";];
value to_yojson : t → Yojson.Safe.t;
[@@@"ocaml.text" "/*";];
module M_of_yojson :
  sig
    type nonrec of_yojson =
      { f : mutable Yojson.Safe.t → Rresult.result t string }
    ;
    value f : of_yojson;
  end
;
[@@@"ocaml.text" "/*";];
value of_yojson : Yojson.Safe.t → Rresult.result t string;
[@@@"ocaml.text" "/*";];
module M_sexp_of_t :
  sig
    type nonrec sexp_of_t = { f : mutable t → Sexplib0.Sexp.t };
    value f : sexp_of_t;
  end
;
[@@@"ocaml.text" "/*";];
value sexp_of_t : t → Sexplib0.Sexp.t;
[@@@"ocaml.text" "/*";];
module M_t_of_sexp :
  sig
    type nonrec t_of_sexp = { f : mutable Sexplib0.Sexp.t → t };
    value f : t_of_sexp;
  end
;
[@@@"ocaml.text" "/*";];
value t_of_sexp : Sexplib0.Sexp.t → t;
[@@@"ocaml.text" "/*";];
module M_pp : sig type nonrec pp = { f : mutable Fmt.t t }; value f : pp; end;
[@@@"ocaml.text" "/*";];
value pp : Fmt.t t;
value show : t → Stdlib.String.t;
[@@@"end"];
type t +=
  [ Help of string[@"rebind_to" Arg.Help;]
  | Bad of string[@"rebind_to" Arg.Bad;]
  | Finally_raised of t[@"rebind_to" Fun.Finally_raised;]
  | Undefined[@"rebind_to" Lazy.Undefined;]
  | Parse_error[@"rebind_to" Parsing.Parse_error;]
  | QueueEmpty[@"rebind_to" Queue.Empty;]
  | Scan_failure of string[@"rebind_to" Scanf.Scan_failure;]
  | StackEmpty[@"rebind_to" Stack.Empty;]
  | Exit[@"rebind_to" Stdlib.Exit;]
  | Match_failure of (string * int * int)[@"rebind_to" Stdlib.Match_failure;]
  | Assert_failure of
      (string * int * int)[@"rebind_to" Stdlib.Assert_failure;]
  | Invalid_argument of string[@"rebind_to" Stdlib.Invalid_argument;]
  | Failure of string[@"rebind_to" Stdlib.Failure;]
  | Not_found[@"rebind_to" Stdlib.Not_found;]
  | Out_of_memory[@"rebind_to" Stdlib.Out_of_memory;]
  | Stack_overflow[@"rebind_to" Stdlib.Stack_overflow;]
  | Sys_error of string[@"rebind_to" Stdlib.Sys_error;]
  | End_of_file[@"rebind_to" Stdlib.End_of_file;]
  | Division_by_zero[@"rebind_to" Stdlib.Division_by_zero;]
  | Sys_blocked_io[@"rebind_to" Stdlib.Sys_blocked_io;]
  | Undefined_recursive_module of
      (string * int * int)[@"rebind_to" Stdlib.Undefined_recursive_module;]
  | StreamFailure[@"rebind_to" Stream.Failure;]
  | Error of string[@"rebind_to" Stream.Error;]
  | Break[@"rebind_to" Sys.Break;]
  | Exc of Ploc.t and t[@"rebind_to" Ploc.Exc;] [@"name" "Ploc.Exc";] ][@@"deriving_inline" (show, sexp, yojson, eq);]
;
[@@@"end"];

