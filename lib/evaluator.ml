open Parser
open Runtime
open Diagnostics

let rec eval (expr : Sexpr.t) (environment : Environment.t) =
  match Sexpr.raw expr with
  | Sexpr.Number n -> Runtime.Number n

  | Sexpr.Symbol s -> (
    (* Search for definition of symbol s in environment *)
    match Environment.lookup environment s with
    | Some value -> value
    | None ->
      (* Symbol is unbound *)
      Errors.unbound_symbol (Sexpr.location expr) s
  )

  (* Empty list *)
  | Sexpr.List [] -> Runtime.List []

  (* List head application *)
  | Sexpr.List (head :: args) ->
    let use_meta = {
      location = Some (Sexpr.location head);
      name = match Sexpr.raw head with
      | Sexpr.Symbol s -> Some s
      | _ -> None;
    } in
    (* Get the definition of func in environment *)
    let func = eval head environment in
    (* Apply the function func *)
    apply func args environment use_meta

and apply value args environment metadata : Value.t =
  (* 
    Placeholder location for when apply metadata is invalid or location can't be determined by definition metadata
  *)
  let apply_loc : Diagnostics.Location.t = 
    { source = __FILE__; line = __LINE__; column = 0; }
  in

  (* 
    Evaluate arguments (Sexpr.t list) before passing them to:
    Native or lambda:
    (print (+ 2 2)) = (+ 2 2) = 4 |> print
  *)
  let eval_args args = List.map (fun arg -> eval arg environment) args in
  let arg_count = List.length args in

  (* Dispatch application type *)
  match value with
  (* OCaml defined function *)
  | Runtime.Native native ->
    (* Build a use place metadata *)
    let use_meta = Metadata.fill_w_fallback metadata native.metadata in

    (* Match argument count against arity  *)
    if not (Applicable.check native.applicable.arity arg_count) then
      Errors.arity_mismatch
        (Metadata.location_or use_meta apply_loc)
        (Metadata.name_or use_meta "unknown-native")
        (native.applicable.args)
        (List.map Sexpr.to_string args)
      ;
    
    (* Apply function body to use metadata, arguments in given environment *)
    native.body use_meta (eval_args args) environment
  
  (* Language SpecialForm implemented in OCaml *)
  (* 
    It receives the arguments as Sexpr.t
    For example def construct (def <name> <value>)
    can't receive <name> already evaluated as it would happen with native 
    or lambda functions, because it wants its symbol information, so it chooses
    to receive the unevaluated expression
  *)
  | Runtime.SpecialForm sform -> 
     (* Build a use place metadata *)
    let use_meta = Metadata.fill_w_fallback metadata sform.metadata in

    (* Match argument count against arity  *)
    if not (Applicable.check sform.applicable.arity arg_count) then
      Errors.arity_mismatch
        (Metadata.location_or use_meta apply_loc) 
        (Metadata.name_or use_meta "unknown-special-form")
        (sform.applicable.args)
        (List.map Sexpr.to_string args)
      ;  
    
    sform.body use_meta args environment

  (* Lisp defined function *)
  | Runtime.Lambda lambda ->
    (* Make sure function arity matches provided arguments *)
    if not (Applicable.check lambda.applicable.arity arg_count) then
      Errors.arity_mismatch
        (Metadata.location_or lambda.metadata apply_loc)
        (Metadata.name_or metadata "anonymous-lambda")
        (lambda.applicable.args)
        (List.map Sexpr.to_string args)
      ;

    (* Create a closure with parent being function closure when defined *)
    let closure = Environment.create (Some lambda.closure) in

    (* Bind lambda.applicable.args to values in the function closure *)
    List.iter2 
      (Environment.define closure) 
      lambda.applicable.args (eval_args args);

    (* Actually execute function body *)
    eval lambda.body closure
  
  (* Not applicable value *)
  | _ ->  
    Errors.not_applicable apply_loc (Value.to_string value)