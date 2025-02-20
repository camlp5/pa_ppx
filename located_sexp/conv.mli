(**pp -syntax camlp5o -package sexplib *)

val apply_converter : (Sexplib.Type.t -> 'a) -> Sexp.t -> 'a

val hashtbl_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) Hashtbl.t

val list_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a list
val sexp_of_list : Ploc.t -> ('a -> Sexp.t) -> 'a list -> Sexp.t

val array_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a array
val sexp_of_array : Ploc.t -> ('a -> Sexp.t) -> 'a array -> Sexp.t

val ref_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a ref
val sexp_of_ref : Ploc.t -> ('a -> Sexp.t) -> 'a ref -> Sexp.t

val option_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a option
val sexp_of_option : Ploc.t -> ('a -> Sexp.t) -> 'a option -> Sexp.t

val unit_of_sexp : Sexp.t -> unit
val sexp_of_unit : Ploc.t -> unit -> Sexp.t

val int_of_sexp : Sexp.t -> int
val sexp_of_int : Ploc.t -> int -> Sexp.t

val string_of_sexp : Sexp.t -> string
val sexp_of_string : Ploc.t -> string -> Sexp.t

val float_of_sexp : Sexp.t -> float
val sexp_of_float : Ploc.t -> float -> Sexp.t
