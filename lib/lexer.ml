open Diagnostics

module Token = struct
  type t =
    | Illegal
    | LParen
    | RParen
    | Symbol of string
    | Number of int

  let to_string = function
    | Illegal -> "Illegal"
    | LParen -> "LParen"
    | RParen -> "RParen"
    | Symbol s -> "Symbol: \"" ^ s ^ "\""
    | Number n -> "Number: " ^ string_of_int n

  let print (tok : t) =
    print_string (to_string tok)
end

type t = 
{
  source  : string;
  pos     : int;

  (* Diagnostics only *)
  loc     : Location.t
}
  
let isalpha = function
  | '(' | ')' | ' ' | '\t' | '\r' | '\n' -> false
  | _ -> true
;;

let isdigit = function
  | c when c >= '0' && c <= '9' -> true
  | _ -> false
;;

let text_to_number = function
  | x -> Token.Number (int_of_string x)
;;

let text_to_symbol = function
  | x -> Token.Symbol x
;;

let advance state =
  { 
    state with 
    pos = state.pos + 1;
    loc = {
      column = state.loc.column + 1;
      line = state.loc.line;
      source = state.loc.source
    }
  }
;;

let rec skip_whitespace state =
  if state.pos >= String.length state.source then
    state
  else
    match String.get state.source state.pos with
    | ' ' | '\t' | '\r' -> skip_whitespace (advance state)
    | '\n' -> skip_whitespace 
      { 
        state with 
        pos = state.pos + 1;
        loc = {
          line = state.loc.line + 1;
          column = 0;
          source = state.loc.source;
        }
      }
    | _ -> state
;;

let read_token_while state predicate transform : ((Token.t * Location.t) * t) =
  let rec loop state =
    if state.pos >= String.length state.source then
      state
    else
      let c = String.get state.source state.pos in

      if predicate c then loop (advance state)
      else state
  in

  let new_state = loop state in
  let text = String.sub state.source state.pos (new_state.pos - state.pos) in

  ((transform text, state.loc), new_state)
;;

let lex (contents : string) ~(source : string) : (Token.t * Location.t) list =
  let rec next_token state =
    let state = skip_whitespace state in

    if state.pos >= String.length state.source then
      []
    else
      let char = String.get state.source state.pos in
      let loc = state.loc in

      match char with
      | '(' -> 
        (Token.LParen, loc) :: (next_token (advance state))

      | ')' -> 
        (Token.RParen, loc) :: (next_token (advance state))

      | c when isdigit c -> 
        let token, new_state = read_token_while state isdigit text_to_number in
        token :: (next_token new_state)
        
      | c when isalpha c ->
        let token, new_state = read_token_while state isalpha text_to_symbol in
        token :: (next_token new_state)

      | i -> 
        raise (
          Diagnostics.Error (
            Diagnostics.Error.Unexpected_Character 
            (state.loc, Printf.sprintf "\'%c\'" i)
          )
        )
  in 
  next_token { 
    source = contents; 
    pos = 0; 
    loc = { line = 1; column = 1; source } 
  }
;;