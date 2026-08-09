open Value

let is_truth = function
  | Number x when x = 0 -> false
  | _ -> true

let is_false = function
  | Number x when x = 0 -> true
  | _ -> false

let builtin_if (args : Sexpr.expr list) (env : env) : value =
  match args with
  | [] -> failwith "Empty if statement"
  | [cond; success] -> (
    if is_truth (Evaluator.eval cond env) then
      Evaluator.eval success env
    else Unit
  )
  | [cond; success; failure] -> (
    if is_truth (Evaluator.eval cond env) then
      Evaluator.eval success env
    else
      Evaluator.eval failure env
  )

  | _ -> failwith "Too many parts for if-statement"

let builtin_def (args : Sexpr.expr list) (env : env) : value =
  match args with
  | [] -> failwith "Missing name, value for def"
  | [name] -> failwith "Missing value for def"
  | [name; value] -> (
    match name with
    | Sexpr.Symbol n -> 
      define env n (Evaluator.eval value env);
      Unit

    | _ -> failwith "Name for def must be a symbol"
  )

  (* TODO: 
    I think all the remainings should be passed to eval 
    but in any case this function does require a lot of logic
    to handle destructuring  
  *)
  | _ -> failwith "Too many parts for def"

let builtin_function (args : Sexpr.expr list) (env : env) : value =
  match args with
  | [Sexpr.List parameters; body] ->
    let params = List.map (
      function
      | Sexpr.Symbol name -> name
      | _ -> failwith "Function parameter name must be a symbol"
    ) parameters
    in Function {
      params;
      body;
      env
    }
  | _ -> failwith "Function expects parameters (list) and body"


let builtin_is_truthy (args : value list) : value =
  if List.for_all is_truth args then Number 1
  else Number 0
;;
let builtin_is_falsy (args : value list) : value =
  if List.for_all is_false args then Number 1
  else Number 0
;;

let builtin_print (values : value list) =
  List.iter (fun v -> Value.print_value v) values;
  Value.Unit
;;

let core = 
  let env = create None in
  define env "if" (SpecialForm builtin_if);
  define env "def" (SpecialForm builtin_def);
  define env "function" (SpecialForm builtin_function);
  define env "is_truthy" (NativeFunction builtin_is_truthy);
  define env "is_falsy" (NativeFunction builtin_is_falsy);
  define env "print" (NativeFunction builtin_print);
  env
;;