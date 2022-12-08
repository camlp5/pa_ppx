(* camlp5o *)
(* pp_MLast.ml,v *)

declare
  type t = exn == ..;
  value show : exn → string;
  value pp : Fmt.t exn;
  declare
    value sexp_of_t : exn → Sexplib0.Sexp.t;
    value t_of_sexp : Sexplib0.Sexp.t → exn;
    value to_yojson : exn → Yojson.Safe.t;
    value of_yojson : Yojson.Safe.t → Rresult.result exn string;
    value equal : exn → exn → bool;
  end;
end;


