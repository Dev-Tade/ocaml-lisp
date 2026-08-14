open Ocaml_lisp.Diagnostics
open Ocaml_lisp.Lexer

let () =
  let tokens = lex "(print (+ (* 2 4) 5))" ~source:"<lexer-test>" in
  List.iter (
    fun (tok, loc) ->
      Token.print tok;
      print_string " at: ";
      Location.print loc;
      print_newline ();
  ) tokens