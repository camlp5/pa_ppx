(* camlp5o *)

type t = [
    `OptionA
  | `OptionB
]

type t' = A | B

module type S = sig
  val test : unit -> string
end

val validate_option : t -> unit
val validate_module_type : (module S) -> unit

