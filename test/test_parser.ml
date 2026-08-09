open Ocaml_lisp

let () =
  let tokens = Lexer.lex "(print (+ (* 2 4) 5))\n(print (+ 1 2))\n($ hello)" in
  List.iter Sexpr.print_expr (Parser.parse tokens);