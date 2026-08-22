open Parser

type value =
  | Unit
  | Number of int
  | Symbol of string
  | List of value list
  | NativeFunction of (metadata -> value list -> value) * metadata
  | SpecialForm of (metadata -> Sexpr.t list -> environment -> value) * metadata
  | Lambda of lambda * metadata
and lambda = 
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
and metadata =
{
  name : string option;
  location : Diagnostics.Location.t option;
}

module Value : sig
  type t = value

  val from_sexpr : Sexpr.t -> t
  val to_sexpr : t -> (Sexpr.t, Diagnostics.Error.t) result

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