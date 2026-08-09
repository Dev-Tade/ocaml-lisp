open Lexer
open Sexpr

type parserState = 
{
  current : expr list;
  stack : expr list list;
}

let parse (tokens : token list) : expr list =
  let rec next_token tokens state =
    match tokens with
    | [] -> ((* No more tokens *) 
      match state.stack with
      | [] -> (* All lists are closed *)
        (* Return reversed list of all parsed expressions *)
        List.rev state.current
      | _ -> failwith "Unexpected end of input: missing ')'"
    )

    | now :: rest ->
      let line = now.line in 
      let column = now.column in
      
      match now.kind with
      | Illegal -> (* Illegal Token *) 
        failwith (Printf.sprintf "Illegal token at %d:%d" line column)
  
      | LParen -> (* Parse a list opening *)
        next_token rest { stack = state.current :: state.stack; current = [] }
  
      | RParen -> (
        match state.stack with
        | [] -> (* Tries closing a list that doesn't exists *)
          failwith (Printf.sprintf "Unexpected ')' at %d:%d" line column)
        
        | parent :: rest_stack -> (* Closes the 'current list being parsed *)
          let closed = Sexpr.List (List.rev state.current) in
          next_token rest { current = closed :: parent; stack = rest_stack }
      )
      
      | Number n -> 
        next_token rest { state with current = Number n :: state.current }
      
      | Symbol s -> 
        next_token rest { state with current = Symbol s :: state.current }
  in 
  next_token tokens { current = []; stack = [] }
;;