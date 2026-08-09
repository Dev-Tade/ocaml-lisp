type value =
  | Unit
  | Number of int
  | Symbol of string
  | List of value list
  | NativeFunction of (value list -> value)
  | SpecialForm of (Sexpr.expr list -> env -> value)
  | Function of {
    params: string list;
    body: Sexpr.expr;
    env: env
  }
and env = 
{ 
  values : (string, value) Hashtbl.t;
  parent : env option;
}

val create : env option -> env
val define : env -> string -> value -> unit
val lookup : env -> string -> value option

val value_to_string : value -> string
val print_value : value -> unit
