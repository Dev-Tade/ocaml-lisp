
type tokenKind =
  | Illegal
  | LParen
  | RParen
  | Symbol of string
  | Number of int

type token = {
  kind: tokenKind;
  line: int;
  column: int;
}

val lex : string -> token list 
val token_to_string : token -> string
val print_token : token -> unit