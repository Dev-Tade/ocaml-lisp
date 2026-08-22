open Diagnostics
open Runtime
open Parser

val eval : Sexpr.t -> Environment.t -> Value.t
val apply : Value.t -> Sexpr.t list -> Environment.t -> metadata -> Value.t