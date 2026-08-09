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
  let env = Value.create (Some Core_env.core) in

  (* Include non core builtins *)
  Value.define env "+" (Value.NativeFunction builtin_add);
  Value.define env "*" (Value.NativeFunction builtin_mul);

  let rec repl () =
    print_string "ocaml-lisp> ";
    flush stdout;

    match read_line () with
    | exception End_of_file ->
      print_endline "EOF"; 
      ()

    | line ->
      (* Empty string *)
      if String.trim line = "" then repl ()
      else 
        try
          let tokens = Lexer.lex line in
          let exprs = Parser.parse tokens in

          List.iter (fun expr ->
            let r = Evaluator.eval expr env in
            print_string "# ";
            Value.print_value r;
          ) exprs;

          repl ()
        with Failure msg ->
          Printf.printf "! Error: %s\n" msg;
          repl ()
  in
  repl ()
