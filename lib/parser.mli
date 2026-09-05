open Diagnostics
open Lexer

module Sexpr : sig  
  type node =
    | Symbol of string
    | Number of int
    | List of t list
  and t = node Location.located

  val raw : t -> node
  val location : t -> Location.t

  val make_symbol : string -> Location.t -> t
  val make_number : int -> Location.t -> t
  val make_list : t list -> Location.t -> t

  val basetype : string
  val typename : node -> string

  val to_string : t -> string
  val print : ?locate:bool -> t -> unit
end

val parse : (Lexer.Token.t * Location.t) list -> Sexpr.t list