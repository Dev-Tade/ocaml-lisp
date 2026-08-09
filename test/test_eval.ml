open Ocaml_lisp

let make_arithmetic_op name initial op values =
  let rec apply values acc =
    match values  with
    | [] -> Value.Number acc

    | Value.Number n :: rest ->
      apply rest (op acc n)
    
    | _ -> failwith
      (Printf.sprintf "Arithmetic Operator '%s' expects only numbers" name)
  in
  apply values initial
;; 

let builtin_add = make_arithmetic_op "+" 0 (fun acc n -> acc + n)
let builtin_mul = make_arithmetic_op "*" 1 (fun acc n -> acc * n)


let () =
  (* let tokens = Lexer.lex "(print (+ (* 2 4) 5))\n(print (+ 1 2))" in *)
  let tokens = Lexer.lex "(def x 3)\n(print ((function (x y) (* y x)) 2 x))" in
  let sexpr = Parser.parse tokens in
  let env = Value.create (Some Core_env.core) in
  Value.define env "+" (NativeFunction builtin_add);
  Value.define env "*" (NativeFunction builtin_mul);

  List.iter (fun expr -> (Value.print_value (Evaluator.eval expr env))) sexpr
