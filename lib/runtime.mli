open Parser

type value =
  | Unit
  | Number of int
  | Symbol of string
  | List of value list
  | NativeFunction of (value list -> value)
  | SpecialForm of (Sexpr.t list -> environment -> value)
  | Function of {
    params: string list;
    body: Sexpr.t;
    env: environment
  }
and environment =
{ 
  values : (string, value) Hashtbl.t;
  parent : environment option;
}
module Value : sig
  type t = value
  val to_string : t -> string
  val print : t -> unit
end
module Environment : sig
  type t = environment
  val create : t option -> t
  val define : t -> string -> Value.t -> unit
  val lookup : t -> string -> Value.t option
end