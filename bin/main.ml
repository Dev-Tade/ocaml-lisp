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

let builtin_add =
  Runtime.Native 
  {
    applicable = Runtime.Applicable.at_least ["x"; "y"];
    metadata = Runtime.Metadata.make "builtin-add" __FILE__ __LINE__ 0;
    body = (fun metadata args environment -> make_arithmetic_op "+" 0 (fun acc n -> acc + n) args);
  }
  
let builtin_mul metadata  args environment = make_arithmetic_op "*" 1 (fun acc n -> acc * n) args

let () =
  let repl_env = Runtime.Environment.create (Some Core.std) in

  (* Include non core builtins *)
  Runtime.Environment.define repl_env "+" builtin_add;
  Runtime.Environment.define repl_env "*" (
    Runtime.Native 
    {
      applicable = Runtime.Applicable.at_least ["x"; "y"];
      metadata   = Runtime.Metadata.make "builtin-mul" __FILE__ __LINE__ 0;
      body       = builtin_mul
    }
  );

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
          let tokens = Lexer.lex line ~source:"<repl>" in
          let exprs = Parser.parse tokens in

          List.iter (fun expr ->
            let r = Evaluator.eval expr repl_env in
            print_string "# ";
            Runtime.Value.print r;
            print_newline ()
          ) exprs;

          repl ()
        with 
        | Diagnostics.Error err ->
          Printf.printf "Error at %s\n" (Diagnostics.Error.to_string err);
          repl()
        | Failure msg ->
          Printf.printf "! Error: %s\n" msg;
          repl ()
  in
  repl ()
