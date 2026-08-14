open Diagnostics

module Token : sig
  type t =
    | Illegal
    | LParen
    | RParen
    | Symbol of string
    | Number of int

  val to_string : t -> string
  val print : t -> unit
end

val lex : string -> source:string -> (Token.t * Location.t) list
