open Parser
open Runtime

let is_truth = function
  | Runtime.Number x when x = 0 -> false
  | _ -> true

let is_false = function
  | Runtime.Number x when x = 0 -> true
  | _ -> false

let builtin_if (args : Sexpr.t list) (env : Environment.t) : Value.t =
  match args with
  | [] -> failwith "Empty if statement"
  | [cond] -> failwith "Empty if statement <then>"
  | [cond; success] -> (
    if is_truth (Evaluator.eval cond env) then
      Evaluator.eval success env
    else Runtime.Unit
  )
  | [cond; success; failure] -> (
    if is_truth (Evaluator.eval cond env) then
      Evaluator.eval success env
    else
      Evaluator.eval failure env
  )

  | _ -> failwith "Too many parts for if-statement"

let builtin_def (args : Sexpr.t list) (env : Environment.t) : value =
  match args with
  | [] -> failwith "Missing name, value for def"
  | [name] -> failwith "Missing value for def"
  | [name; value] -> (
    match Sexpr.raw name with
    | Sexpr.Symbol name -> 
      Environment.define env name (Evaluator.eval value env);
      Unit

    | _ -> failwith "Name for def must be a symbol"
  )

  (* TODO: 
    I think all the remainings should be passed to eval 
    but in any case this function does require a lot of logic
    to handle destructuring  
  *)
  | _ -> failwith "Too many parts for def"

let builtin_function (args : Sexpr.t list) (environment : Environment.t) : Value.t =
  match args with
  | [Sexpr.List parameters, _; body] ->
    let params = List.map (
      function
      | Sexpr.Symbol name, _ -> name
      | _ -> failwith "Function parameter name must be a symbol"
    ) parameters
    in Function {
      closure = environment;
      params;
      body;
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
  List.iter (fun v -> Value.print v; print_newline ()) values;
  Runtime.Unit
;;

let std = 
  let std = Environment.create None in
  Environment.define std "if" (SpecialForm builtin_if);
  Environment.define std "def" (SpecialForm builtin_def);
  Environment.define std "function" (SpecialForm builtin_function);
  Environment.define std "is_truthy" (NativeFunction builtin_is_truthy);
  Environment.define std "is_falsy" (NativeFunction builtin_is_falsy);
  Environment.define std "print" (NativeFunction builtin_print);
  std
;;