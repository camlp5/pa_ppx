(* camlp5o *)
(* pp_MLast.ml,v *)

declare
  type t = exn == ..;
  value show : exn → string;
  value pp : Fmt.t exn;
  declare end;
end;


