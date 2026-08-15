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
        Diagnostics.Message
        (Diagnostics.Msg.Unbound_Symbol (Sexpr.location expr, s))
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
    | Runtime.NativeFunction native -> 
      (* 
        Translate Sexpr.t arguments to Runtime.Value.t 
        for Runtime.NativeFunction 
        This means arguments to native are evaluated before hand as:
        (print (+ 2 2)) = (+ 2 2) |> (native print)
      *)
      let args = List.map (fun expr -> eval expr environment) args in
      native args
    
    (* Language SpecialForm implemented in OCaml *)
    (* 
      It receives the arguments as Sexpr.t
      For example (def <name> <value>)
      It can't receive <name> already evaluated because 
      it wants its symbol information, so it chooses
      to receive the unevaluated expression, then it has <name>
      as Symbol s, and does eval <value> environment
    *)
    | Runtime.SpecialForm sform -> sform args environment

    (* Lisp defined function *)
    | Runtime.Function fnc ->
      (* Make sure function arity matches provided arguments *)
      if List.length fnc.params <> List.length args then
        raise (
          Diagnostics.Message
          (Diagnostics.Msg.Arity_Mismatch (
            Sexpr.location head, 
            (Printf.sprintf "%s expected %d, but got %d"
              (Value.to_string func)
              (List.length fnc.params)
              (List.length args))
          ))
        );

      (* Create a closure with parent being function closure when defined *)
      let closure = Environment.create (Some fnc.closure) in
      (* 
        Translate Sexpr.t arguments to Value.t for Runtime.Function 
        This means arguments to fnc are evaluated before hand as:
        (double (+ 2 2)) = (+ 2 2) |> (function double)
      *)
      let values = List.map (fun expr -> eval expr environment) args in

      (* Bind fnc.params to values in the function closure *)
      List.iter2 
        (fun name value -> Environment.define closure name value) 
        fnc.params values;

      eval fnc.body closure
    
    (* Not applicable function func *)
    | _ ->  
      raise (
        Diagnostics.Message
        (Diagnostics.Msg.Not_Callable 
        (Sexpr.location expr, Value.to_string func))
      )
  )
;;