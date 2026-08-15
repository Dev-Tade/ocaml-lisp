open Ocaml_lisp

let () =
  let tokens = Lexer.lex 
    "(print (+ (* 2 4) 5))\n(print (+ 1 2))\n($ hello)" 
    ~source:"<parser-test>" 
  in
  List.iter (Parser.Sexpr.print) (Parser.parse tokens);