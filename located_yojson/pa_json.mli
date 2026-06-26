(**pp -syntax camlp5o -package camlp5 *)

val g : Grammar.g
val json : Json0.t Grammar.Entry.e
val json_eoi : Json0.t Grammar.Entry.e
val json_list_eoi : Json0.t list Grammar.Entry.e

val with_input_file : (char Stream.t -> 'a) -> file:string -> 'a

module type PAHELPER = sig
  type t = 'a
  val entry : t Grammar.Entry.e
  val parse : char Stream.t -> t
  val of_string : string -> t
  val input : in_channel -> t
  val load : file:string -> t
end

module Json : (PAHELPER with type t = Json0.t)
module JsonEOI : (PAHELPER with type t = Json0.t)
module JsonList : (PAHELPER with type t = Json0.t list)
module JsonListEOI : (PAHELPER with type t = Json0.t list)
