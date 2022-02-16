(* camlp5o *)
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

  let bp = bp - bol in
  let ep = ep - bol in
  Fmt.(pf ppf "<%a:%d:%d-%d>" (quote string) fname line bp ep)

let pp1_loc ppf x = Fmt.(const string "<loc>" ppf ())

let pp_loc_verbose = ref false

let pp ppf x =
  if !pp_loc_verbose then
    pp0_loc ppf x
  else
    pp1_loc ppf x

let equal (x : t) y = x = y
let to_yojson (x : t) =
  let s = Fmt.(str "%a" pp x) in
  `String s
let sexp_of_t (x : t) =
  let s = Fmt.(str "%a" pp x) in
  Sexplib0.Sexp.Atom s

end


type t = exn = .. [@@deriving show, sexp_of, to_yojson, eq]

type t +=
    Help of string [@rebind_to Arg.Help][@name "Arg.Help"]
  | Bad of string [@rebind_to Arg.Bad][@name "Arg.Bad"]
  | Finally_raised of t [@rebind_to Fun.Finally_raised][@name "Fun.Finally_raised"]
  | Undefined [@rebind_to Lazy.Undefined][@name "Lazy.Undefined"]
  | Parse_error [@rebind_to Parsing.Parse_error][@name "Parsing.Parse_error"]
  | QueueEmpty [@rebind_to Queue.Empty][@name "Queue.Empty"]
  | Scan_failure of string [@rebind_to Scanf.Scan_failure][@name "Scanf.Scan_failure"]
  | StackEmpty [@rebind_to Stack.Empty][@name "Stack.Empty"]
  | Exit [@rebind_to Stdlib.Exit][@name "Stdlib.Exit"]
  | Match_failure of (string * int * int) [@rebind_to Stdlib.Match_failure][@name "Stdlib.Match_failure"]
  | Assert_failure of (string * int * int) [@rebind_to Stdlib.Assert_failure][@name "Stdlib.Assert_failure"]
  | Invalid_argument of string [@rebind_to Stdlib.Invalid_argument][@name "Stdlib.Invalid_argument"]
  | Failure of string [@rebind_to Stdlib.Failure][@name "Stdlib.Failure"]
  | Not_found [@rebind_to Stdlib.Not_found][@name "Stdlib.Not_found"]
  | Out_of_memory [@rebind_to Stdlib.Out_of_memory][@name "Stdlib.Out_of_memory"]
  | Stack_overflow [@rebind_to Stdlib.Stack_overflow][@name "Stdlib.Stack_overflow"]
  | Sys_error of string [@rebind_to Stdlib.Sys_error][@name "Stdlib.Sys_error"]
  | End_of_file [@rebind_to Stdlib.End_of_file][@name "Stdlib.End_of_file"]
  | Division_by_zero [@rebind_to Stdlib.Division_by_zero][@name "Stdlib.Division_by_zero"]
  | Sys_blocked_io [@rebind_to Stdlib.Sys_blocked_io][@name "Stdlib.Sys_blocked_io"]
  | Undefined_recursive_module of (string * int * int) [@rebind_to Stdlib.Undefined_recursive_module][@name "Stdlib.Undefined_recursive_module"]
  | StreamFailure [@rebind_to Stream.Failure][@name "Stream.Failure"]
  | Error of string [@rebind_to Stream.Error][@name "Stream.Error"]
  | Break [@rebind_to Sys.Break][@name "Sys.Break"]
  | Exc of Ploc.t * t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
[@@deriving show, sexp_of, to_yojson, eq]
;;

let print_exn exn = Some (show exn) ;;
Printexc.register_printer print_exn ;;

ELSE
type t = exn = ..
let show _ = "<exn>"
let pp pps _ = Fmt.(pf pps "<exn>")
IFDEF FAT THEN
let sexp_of_t _ = failwith "no sexp marshallers compiled in yet"
let t_of_sexp _ = failwith "no sexp marshallers compiled in yet"
let to_yojson _ = failwith "no yojson marshaller compiled in yet"
let of_yojson _ = failwith "no yojson marshaller compiled in yet"
let equal _ _ = failwith "no derived equality compiled in yet"
END
END


