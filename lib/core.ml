open Parser
open Runtime
open Diagnostics

let is_truth = function
  | Runtime.Number x when x = 0 -> false
  | _ -> true

let is_false = function
  | Runtime.Number x when x = 0 -> true
  | _ -> false

let builtin_if =
  let builtin_if metadata args environment =
    match args with
    | [cond; success] ->
      if is_truth (Evaluator.eval cond environment) then
        Evaluator.eval success environment
      else Runtime.Unit
    
    | [cond; success; failure] -> 
      if is_truth (Evaluator.eval cond environment) then
        Evaluator.eval success environment
      else
        Evaluator.eval failure environment

    | _ -> Errors.unreacheable 
      (Location.make __FILE__ __LINE__ 0)
      "builtin-if(0): arguments mismatch"
  in

  SpecialForm 
  {
    applicable = Applicable.range 2 ["condition"; "then"; "else"];
    metadata   = Metadata.make "builtin-if" __FILE__ __LINE__ 0;
    body       = builtin_if;
  }

let builtin_def =
  let builtin_def metadata args environment =
    match args with
    | [name; value] -> (
      match Sexpr.raw name with
      | Sexpr.Symbol binding_name -> 
        let binding_value = 
          match Evaluator.eval value environment with
          
          (* Wrap Runtime.Lambda to use def binding_name as metadata name *)
          | Runtime.Lambda lambda -> 
            Runtime.Lambda 
            {
              lambda with 
              metadata = { lambda.metadata with name = Some binding_name }
            }
          
          (* Return the value as it is *)
          | value -> value 
        in
        
        Environment.define environment binding_name binding_value;
        Unit

      | _ -> failwith "Name for def must be a symbol"
    )

    | _ -> Errors.unreacheable
      (Location.make __FILE__ __LINE__ 0)
      "builtin-def(0): arguments mismatch"
  in

  SpecialForm
  {
    applicable = Applicable.fixed ["name"; "value"];
    metadata   = Metadata.make "builtin-def" __FILE__ __LINE__ 0;
    body       = builtin_def;
  }

let builtin_lambda =
  let builtin_lambda metadata args environment =
    match args with
    | [arguments; body] ->
      let arg_names = 
        match Sexpr.raw arguments with
        | Sexpr.List arguments -> List.map 
          begin function
          | Sexpr.Symbol name, _ -> name
          | _, loc -> failwith "function parameter must be a symbol"
          end 
          arguments
        | _ -> failwith "Lambda expects `arguments to be a list"
      in 
      Lambda
      {
        applicable = Applicable.fixed arg_names;
        metadata   = {metadata with name = None};
        closure    = environment;
        body       = body;
      }

    | _ -> Errors.unreacheable
      (Location.make __FILE__ __LINE__ 0)
      "builtin-lambda(0): arguments mismatch"
  in

  SpecialForm
  {
    applicable = Applicable.fixed ["arguments"; "body"];
    metadata   = Metadata.make "builtin-lambda" __FILE__ __LINE__ 0;
    body       = builtin_lambda;
  }

let builtin_quote =
  let builtin_quote (metadata : Metadata.t) args (environment : Environment.t) =
    match args with
    | [expr] -> Value.from_sexpr expr
    | _ -> Errors.unreacheable
      (Location.make __FILE__ __LINE__ 0)
      "builtin-quote(0): arguments mismatch"
  in

  SpecialForm
  {
    applicable = Applicable.fixed ["expression"];
    metadata   = Metadata.make "builtin-quote" __FILE__ __LINE__ 0;
    body       = builtin_quote;
  }

let builtin_eval =
  let builtin_eval (metadata : Metadata.t) args (environment : Environment.t) =
    match args with
    | [value] -> 
      begin match Value.to_sexpr value with
      | Ok res -> Evaluator.eval res environment
      | Error (loc, from_type) -> Errors.invalid_conversion
        loc from_type "Sexpr.t"
      end
      
    | _ -> Errors.unreacheable
      (Location.make __FILE__ __LINE__ 0)
      "builtin-eval(0): arguments mismatch"
  in

  Native
  {
    applicable = Applicable.fixed ["expression"];
    metadata   = Metadata.make "builtin-eval" __FILE__ __LINE__ 0;
    body       = builtin_eval;
  }

let builtin_is_truthy =
  let builtin_is_truthy metadata args environment =
    if List.for_all is_truth args then Number 1
    else Number 0
  in

  Native
  {
    applicable = Applicable.at_least ["x"];
    metadata   = Metadata.make "builtin-is-truthy" __FILE__ __LINE__ 0;
    body       = builtin_is_truthy
  }

let builtin_is_falsy =
  let builtin_is_falsy metadata args environment =
    if List.for_all is_false args then Number 1
    else Number 0
  in

  Native
  {
    applicable = Applicable.at_least ["x"];
    metadata   = Metadata.make "builtin-is-falsy" __FILE__ __LINE__ 0;
    body       = builtin_is_falsy
  }

let builtin_print =
  let builtin_print metadata args environment =
    List.iter (fun v -> Value.print v; print_newline ()) args;
    Runtime.Unit
  in

  Native
  {
    applicable = Applicable.at_least ["values..."];
    metadata   = Metadata.make "builtin-print" __FILE__ __LINE__ 0;
    body       = builtin_print
  }

let std = 
  let std = Environment.create None in
  Environment.define std "if" builtin_if;
  Environment.define std "def" builtin_def;
  Environment.define std "lambda" builtin_lambda;
  Environment.define std "quote" builtin_quote;
  Environment.define std "eval" builtin_eval;
  Environment.define std "is_truthy" builtin_is_truthy;
  Environment.define std "is_falsy" builtin_is_falsy;
  Environment.define std "print" builtin_print;
  std
;;