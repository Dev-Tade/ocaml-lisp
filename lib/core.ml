open Parser
open Runtime

let is_truth = function
  | Runtime.Number x when x = 0 -> false
  | _ -> true

let is_false = function
  | Runtime.Number x when x = 0 -> true
  | _ -> false

let builtin_if =
  let impl metadata args environment =
    match args with
    | [] -> failwith "Empty if statement"
    | [cond] -> failwith "Empty if statement <then>"
    | [cond; success] -> (
      if is_truth (Evaluator.eval cond environment) then
        Evaluator.eval success environment
      else Runtime.Unit
    )
    | [cond; success; failure] -> (
      if is_truth (Evaluator.eval cond environment) then
        Evaluator.eval success environment
      else
        Evaluator.eval failure environment
    )

    | _ -> failwith "Too many parts for if-statement"
  in

  SpecialForm (Metadata.make "if" __FILE__ __LINE__ 0, impl)

let builtin_def =
  let impl metadata args environment =
    match args with
    | [] -> failwith "Missing name, value for def"
    | [name] -> failwith "Missing value for def"
    | [name; value] -> (
      match Sexpr.raw name with
      | Sexpr.Symbol binding_name -> 
        let binding_value = 
          match Evaluator.eval value environment with
          (* Wrap Runtime.Lambda to use def binding_name as metadata name *)
          | Runtime.Lambda (base_meta, func) -> 
            Runtime.Lambda ({base_meta with name = Some binding_name}, func)
          (* Return the value as it is *)
          | value -> value 
        in
        
        Environment.define environment binding_name binding_value;
        Unit

      | _ -> failwith "Name for def must be a symbol"
    )

    (* TODO: 
      I think all the remainings should be passed to eval 
      but in any case this function does require a lot of logic
      to handle destructuring  
    *)
    | _ -> failwith "Too many parts for def"
  in

  SpecialForm (Metadata.make "def" __FILE__ __LINE__ 0, impl)

let builtin_lambda =
  let impl metadata args environment =
    match args with
    | [Sexpr.List parameters, _; body] ->
      let params = List.map (
        function
        | Sexpr.Symbol name, _ -> name
        | _ -> failwith "Function parameter name must be a symbol"
      ) parameters
      in Lambda ({metadata with name = None}, {
        closure = environment;
        params;
        body;
      })
    | _ -> failwith "Lambda expects parameters (list) and body"
  in

  SpecialForm (Metadata.make "lambda" __FILE__ __LINE__ 0, impl)


let builtin_is_truthy =
  let impl metadata args =
    if List.for_all is_truth args then Number 1
    else Number 0
  in

  NativeFunction (Metadata.make "is_truthy" __FILE__ __LINE__ 0, impl)

let builtin_is_falsy =
  let impl metadata args =
    if List.for_all is_false args then Number 1
    else Number 0
  in

  NativeFunction (Metadata.make "is_falsy" __FILE__ __LINE__ 0, impl)

let builtin_print =
  let impl metadata args =
    List.iter (fun v -> Value.print v; print_newline ()) args;
    Runtime.Unit
  in

  NativeFunction (Metadata.make "print" __FILE__ __LINE__ 0, impl)

let std = 
  let std = Environment.create None in
  Environment.define std "if" builtin_if;
  Environment.define std "def" builtin_def;
  Environment.define std "lambda" builtin_lambda;
  Environment.define std "is_truthy" builtin_is_truthy;
  Environment.define std "is_falsy" builtin_is_falsy;
  Environment.define std "print" builtin_print;
  std
;;