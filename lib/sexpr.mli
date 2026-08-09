
type expr =
  | Symbol of string
  | Number of int
  | List of expr list

val print_expr : expr -> unit

val is_number : expr -> bool
val is_symbol : expr -> bool
val is_list : expr -> bool