
type tokenKind =
  | Illegal
  | LParen
  | RParen
  | Symbol of string
  | Number of int

type token = 
{
  kind: tokenKind;
  line: int;
  column: int;
}

type lexerState = 
{
  source  : string;
  pos     : int;
  line    : int;
  column  : int;
}

let string_of_token_value = function
  | Symbol s -> "'" ^ s ^ "'"
  | Number n -> string_of_int n
  | _ -> ""
;;

let string_of_token_kind = function
  | Illegal -> "Illegal"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Symbol _ -> "Symbol"
  | Number _ -> "Number"
;;

let token_to_string (token : token) =
  Printf.sprintf 
    "%s(%d:%d): %s" 
    (string_of_token_kind token.kind) 
    token.line token.column
    (string_of_token_value token.kind)
;;

let print_token token =
  print_endline (token_to_string token)
;;

let make_token kind state =
  { kind ; line = state.line; column = state.column }
;;

let isalpha = function
  | '(' | ')' | ' ' | '\t' | '\r' | '\n' -> false
  | _ -> true
;;

let isdigit = function
  | c when c >= '0' && c <= '9' -> true
  | _ -> false
;;

let text_to_number = function
  | x -> Number (int_of_string x)
;;

let text_to_symbol = function
  | x -> Symbol x
;;

let advance state =
  { state with pos = state.pos + 1; column = state.column + 1; }
;;

let rec skip_whitespace(state : lexerState) : lexerState =
  if state.pos >= String.length state.source then
    state
  else
    match String.get state.source state.pos with
    | ' ' | '\t' | '\r' -> skip_whitespace (advance state)
    | '\n' -> skip_whitespace 
      { state with pos = state.pos + 1; line = state.line + 1; column = 0; }
    | _ -> state
;;

let read_token_while state predicate transform : (token * lexerState) =
  let rec loop state =
    if state.pos >= String.length state.source then
      state
    else
      let c = String.get state.source state.pos in

      if predicate c then loop (advance state)
      else state
  in

  let out_state = loop state in
  let text = String.sub state.source state.pos (out_state.pos - state.pos) in

  let line = state.line in
  let column = state.column in
  ({ kind = (transform text); line; column; }, out_state)
;;

let lex (source : string) : token list =
  let rec next_token state : token list =
    let state = skip_whitespace state in

    if state.pos >= String.length state.source then
      []
    else
      let char = String.get state.source state.pos in

      match char with
      | '(' -> 
        (make_token LParen state) :: (next_token (advance state))

      | ')' -> 
        (make_token RParen state) :: (next_token (advance state))

      | c when isdigit c -> 
        let token, new_state = read_token_while state isdigit text_to_number in
        token :: (next_token new_state)
        
      | c when isalpha c ->
        let token, new_state = read_token_while state isalpha text_to_symbol in
        token :: (next_token new_state)

      | i -> 
        failwith (Printf.sprintf "Illegal character: '%c' at %d:%d"
        i state.line state.column)
  in 
  next_token { source; pos = 0; line = 1; column = 0; }
;;