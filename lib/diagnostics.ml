
module Location = struct
  type t = {
    line:   int;
    column: int;
    source: string;
  }

  type 'a located = 'a * t 

  let to_string (loc : t) : string =
    Printf.sprintf "%s:%d:%d" 
      loc.source 
      loc.line 
      loc.column
    
  let print (loc: t) : unit =
    print_string (to_string loc)
end

module Error = struct
  type t =
    (* Lexer Errors *)
    | Unexpected_Character of Location.t * string
    (* Parser Errors *)
    | Unexpected_Token of Location.t * string
    | Unexpected_EndOfInput of Location.t * string
    (* Evaluator/Runtime Errors *)
    | Unbound_Symbol of Location.t * string
    | Not_Applicable of Location.t * string
    | Arity_Mismatch of Location.t * string

  let to_string (msg : t) : string =
    match msg with
    | Unexpected_Character (loc, reason) ->
      Printf.sprintf "%s: Unexpected character: %s"
        (Location.to_string loc)
        reason

    | Unexpected_Token (loc, reason) ->
      Printf.sprintf "%s: Unexpected token: %s"
        (Location.to_string loc)
        reason
    
    | Unexpected_EndOfInput (loc, reason) ->
      Printf.sprintf "%s: Unexpected end of input: %s"
        (Location.to_string loc)
        reason
    
    | Unbound_Symbol (loc, reason) ->
      Printf.sprintf "%s: Unbound symbol: %s"
        (Location.to_string loc)
        reason
    
    | Not_Applicable (loc, reason) ->
      Printf.sprintf "%s: Not callable: %s"
        (Location.to_string loc)
        reason

    | Arity_Mismatch (loc, reason) ->
      Printf.sprintf "%s: Arity mismatch: %s"
        (Location.to_string loc)
        reason
end

exception Error of Error.t