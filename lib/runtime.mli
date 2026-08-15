open Parser

type metadata =
{
  name : string option;
  location : Diagnostics.Location.t option;
}

type value =
  | Unit
  | Number of int
  | Symbol of string
  | List of value list
  | NativeFunction of metadata * (metadata -> value list -> value)
  | SpecialForm of metadata * (metadata -> Sexpr.t list -> environment -> value)
  | Function of metadata * runtime_function
and runtime_function = 
{
  closure: environment;
  params: string list;
  body: Sexpr.t;
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

module Metadata : sig
  type t = metadata
  val none : t
  val make : string -> string -> int -> int -> t
  val to_string : t -> string
  val print : t -> unit
end