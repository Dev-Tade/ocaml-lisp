
let rec eval (expr : Sexpr.expr) (env : Value.env) : Value.value =
  match expr with
  | Sexpr.Number n -> Value.Number n
  | Sexpr.Symbol s -> (
    match Value.lookup env s with
    | Some value -> value
    | None -> failwith ("Unbound symbol: " ^ s)
  )

  (* Empty list *)
  | Sexpr.List [] -> Value.List []
  | Sexpr.List (op :: args) -> (
    let op = eval op env in
    match op with
    | NativeFunction fn -> 
      let args = 
        List.map (fun arg -> eval arg env) args
      in
      fn args
    
    | SpecialForm fn -> fn args env

    | Function fn ->
      let closure = Value.create (Some fn.env) in
      let values = List.map (fun a -> eval a env) args in
      List.iter2 
        (fun name value -> Value.define closure name value) 
        fn.params values;
      eval fn.body closure
    
    
    | _ ->  
      failwith ("Head of list is not an applicable: " ^ 
      Value.value_to_string op)
  )
;;