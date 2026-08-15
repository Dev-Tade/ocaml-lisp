open Ocaml_lisp

let make_arithmetic_op name initial op values =
  let rec apply values acc =
    match values  with
    | [] -> Runtime.Number acc

    | Runtime.Number n :: rest ->
      apply rest (op acc n)
    
    | _ -> failwith
      (Printf.sprintf "Arithmetic Operator '%s' expects only numbers" name)
  in
  apply values initial
;; 

let builtin_add = make_arithmetic_op "+" 0 (fun acc n -> acc + n)
let builtin_mul = make_arithmetic_op "*" 1 (fun acc n -> acc * n)


let () =
  let tokens = Lexer.lex 
    "(+ (* 2 3 4) (+ 2 3 4 5))" 
    ~source:"<evaluator-test>"
  in

  let sexpr = Parser.parse tokens in
  
  let env = Runtime.Environment.create None in
  Runtime.Environment.define env "+" (NativeFunction builtin_add);
  Runtime.Environment.define env "*" (NativeFunction builtin_mul);

  List.iter (
    fun expr -> (
      Runtime.Value.print (Evaluator.eval expr env);
      print_newline()
    )
  ) sexpr
