open Ocaml_lisp.Sexpr
  
let () =
  let expr = List [
    Symbol "print";
    List [
      Symbol "+";
      List [
        Symbol "*";
        Number 2;
        Number 4;
      ];
      Number 5;
    ];
  ] in

  print_expr expr;