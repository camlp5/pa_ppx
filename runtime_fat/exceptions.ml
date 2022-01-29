(* camlp5o *)
(* pp_MLast.ml,v *)

declare
  type t = exn == ..;
  value show _ = "<exn>";
  value pp pps _ = Fmt.(pf pps "<exn>");
  declare
    value sexp_of_t _ = failwith "no sexp marshallers compiled in yet";
    value t_of_sexp _ = failwith "no sexp marshallers compiled in yet";
    value to_yojson _ = failwith "no yojson marshaller compiled in yet";
    value of_yojson _ = failwith "no yojson marshaller compiled in yet";
    value equal _ _ = failwith "no derived equality compiled in yet";
  end;
end;


