module Location : sig
  type t = { line : int; column : int; source : string; }
  type 'a located = 'a * t
  
  val make : string -> int -> int -> t 
  
  val to_string : t -> string
  val print : t -> unit
end


module Errors : sig
    type t
    exception Error of t
    
    val to_string : t -> string

    (* General Purpose *)
    val unreacheable : Location.t -> string -> 'a
    (* Lexer *)
    val unexpected_character : Location.t -> char -> 'a
    (* Parser *)
    val unexpected_token : Location.t -> string -> 'a
    val unexpected_eof : Location.t -> string -> 'a
    (* Evaluator/Runtime *)
    val invalid_type : Location.t -> string -> string -> string -> 'a
    val invalid_conversion : Location.t -> string -> string -> 'a
    val unbound_symbol : Location.t -> string -> 'a
    val not_applicable : Location.t -> string -> 'a
    val arity_mismatch : Location.t -> string -> string list -> string list -> 'a

  end
