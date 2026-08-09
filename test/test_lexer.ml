open Ocaml_lisp.Lexer

let () =
  let tokens = lex "(print (+ (* 2 4) 5))" in
  List.iter print_token tokens