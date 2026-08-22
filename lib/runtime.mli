open Parser

type arity =
| Fixed of int
| Range of int * int
| AtLeast of int

type applicable =
{
  arity : arity;
  args  : string list;
}

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
| Native of native
| SpecialForm of special_form
| Lambda of lambda
and native =
{
  applicable : applicable;
  metadata   : metadata;
  body       : metadata -> value list -> environment -> value;
}
and special_form =
{
  applicable : applicable;
  metadata   : metadata;
  body       : metadata -> Sexpr.t list -> environment -> value;
}
and lambda = 
{
  applicable : applicable;
  metadata   : metadata;
  closure    : environment;
  body       : Sexpr.t;
}
and environment =
{ 
  values : (string, value) Hashtbl.t;
  parent : environment option;
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

module Applicable : sig
  type t = applicable

  val fixed : string list -> t
  val range : int -> string list -> t
  val at_least : string list -> t

  val check : arity -> int -> bool

  val to_string : t -> string
  val print : t -> unit
end

module Metadata : sig
  type t = metadata
  val none : t
  val make : string -> string -> int -> int -> t

  val fill_w_fallback : t -> t -> t

  val name_or : t -> string -> string
  val location_or : t -> Diagnostics.Location.t -> Diagnostics.Location.t

  val to_string : t -> string
  val print : t -> unit
end