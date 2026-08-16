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
  (Runtime.NativeFunction (
    (fun meta -> make_arithmetic_op "+" 0 (fun acc n -> acc + n)),
    {
      name = Some "builtin_add"; 
      location = Some { 
        source = __FILE__;
        line = __LINE__;
        column = 0;
      }
    }
    )
  )
  
let builtin_mul meta = make_arithmetic_op "*" 1 (fun acc n -> acc * n)

let () =
  let repl_env = Runtime.Environment.create (Some Core.std) in

  (* Include non core builtins *)
  Runtime.Environment.define repl_env "+" builtin_add;
  Runtime.Environment.define repl_env "*" (
    NativeFunction (
      builtin_mul, {
        name = Some "builtin_mul";
        location = Some {
          source = __FILE__;
          line = __LINE__;
          column = 0;
        }
      }
    )
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
