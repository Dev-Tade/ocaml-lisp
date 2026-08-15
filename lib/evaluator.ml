open Parser
open Runtime

let rec eval expr environment =
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

    (* Apply the function func depending on type *)
    match func with
    (* OCaml defined function *)
    | Runtime.NativeFunction (meta, native) -> 
      (* 
        Translate Sexpr.t arguments to Runtime.Value.t 
        for Runtime.NativeFunction 
        This means arguments to native are evaluated before hand as:
        (print (+ 2 2)) = (+ 2 2) |> (native print)
      *)
      let args = List.map (fun expr -> eval expr environment) args in
      
      (* Replace metadata location with head application location *)
      let use_meta = { meta with location = Some (Sexpr.location head) } in
      native use_meta args
    
    (* Language SpecialForm implemented in OCaml *)
    (* 
      It receives the arguments as Sexpr.t
      For example (def <name> <value>)
      It can't receive <name> already evaluated because 
      it wants its symbol information, so it chooses
      to receive the unevaluated expression, then it has <name>
      as Symbol s, and does eval <value> environment
    *)
    | Runtime.SpecialForm (meta, special_form) -> 
      (* Replace metadata location with head application location *)
      let use_meta = { meta with location = Some (Sexpr.location head) } in
      special_form use_meta args environment

    (* Lisp defined function *)
    | Runtime.Lambda (meta, lambda) ->
      (* Make sure function arity matches provided arguments *)
      if List.length lambda.params <> List.length args then
        raise (
          Diagnostics.Error
          (Diagnostics.Error.Arity_Mismatch (
            Sexpr.location head, 
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
        Translate Sexpr.t arguments to Value.t for Runtime.Function 
        This means arguments to fnc are evaluated before hand as:
        (double (+ 2 2)) = (+ 2 2) |> (function double)
      *)
      let values = List.map (fun expr -> eval expr environment) args in

      (* Bind fnc.params to values in the function closure *)
      List.iter2 
        (fun name value -> Environment.define closure name value) 
        lambda.params values;

      eval lambda.body closure
    
    (* Not applicable function func *)
    | _ ->  
      raise (
        Diagnostics.Error (
          Diagnostics.Error.Not_Applicable 
          (Sexpr.location expr, Value.to_string func)
        )
      )
  )
;;