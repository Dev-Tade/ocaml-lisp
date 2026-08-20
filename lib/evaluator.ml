open Parser
open Runtime

let rec eval (expr : Sexpr.t) (environment : Environment.t) =
  match Sexpr.raw expr with
  | Sexpr.Number n -> Runtime.Number n

  | Sexpr.Symbol s -> (
    (* Search for definition of symbol s in environment *)
    match Environment.lookup environment s with
    | Some value -> value
    | None ->
      (* Symbol is unbound *)
      raise (
        Diagnostics.Error
        (Diagnostics.Error.Unbound_Symbol (Sexpr.location expr, s))
      )
  )

  (* Empty list *)
  | Sexpr.List [] -> Runtime.List []

  (* List head application *)
  | Sexpr.List (head :: args) -> (
    (* Get the definition of func in environment *)
    let func = eval head environment in
    (* Apply the function func *)
    apply func args environment (Sexpr.location head)
  )

and apply value args environment apply_loc : Value.t =
  (* Dispatch application type *)
  match value with
  (* OCaml defined function *)
  | Runtime.NativeFunction (native, meta) -> 
    (* 
      Evaluate arguments (Sexpr.t list)
      Arguments to native functions are evaluated:
      (print (+ 2 2)) = (+ 2 2) = 4 |> print
    *)
    let args = List.map (fun arg -> eval arg environment) args in
    
    (* Replace metadata location with application location *)
    let use_meta = { meta with location = Some apply_loc } in
    native use_meta args
  
  (* Language SpecialForm implemented in OCaml *)
  (* 
    It receives the arguments as Sexpr.t
    For example def construct (def <name> <value>)
    can't receive <name> already evaluated as it would happen with native 
    or lambda functions, because it wants its symbol information, so it chooses
    to receive the unevaluated expression
  *)
  | Runtime.SpecialForm (special_form, meta) -> 
    (* Replace metadata location with application location *)
    let use_meta = { meta with location = Some apply_loc } in
    special_form use_meta args environment

  (* Lisp defined function *)
  | Runtime.Lambda (lambda, meta) ->
    (* Make sure function arity matches provided arguments *)
    if List.length lambda.params <> List.length args then
      raise (
        Diagnostics.Error
        (Diagnostics.Error.Arity_Mismatch (
          apply_loc, 
          (Printf.sprintf "%s expects %d arguments but got %d"
            (match meta.name with 
            | Some name -> name 
            | None -> "anonymous lambda")
            (List.length lambda.params)
            (List.length args))
        ))
      );

    (* Create a closure with parent being function closure when defined *)
    let closure = Environment.create (Some lambda.closure) in
    (* 
      Evaluate arguments (Sexpr.t list) 
      Arguments to lambda are evaluated:
      (double (+ 2 2)) = (+ 2 2) = 4 |> double
    *)
    let values = List.map (fun expr -> eval expr environment) args in

    (* Bind lambda.params to values in the function closure *)
    List.iter2 
      (fun name value -> Environment.define closure name value) 
      lambda.params values;

    (* Actually execute function body *)
    eval lambda.body closure
  
  (* Not applicable value *)
  | _ ->  
    raise (
      Diagnostics.Error (
        Diagnostics.Error.Not_Applicable 
        (apply_loc, Value.to_string value)
      )
    )