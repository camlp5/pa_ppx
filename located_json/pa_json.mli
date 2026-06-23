(**pp -syntax camlp5o -package camlp5 *)

val g : Grammar.g
val json_eoi : Json0.t Grammar.Entry.e
val parse_json_eoi : char Stream.t -> Json0.t
val of_string : string -> Json0.t
val input_json : in_channel -> Json0.t
val load_json : string -> Json0.t
