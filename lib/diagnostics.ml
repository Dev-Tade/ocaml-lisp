
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
    (* General Purpose *)
    | Unreacheable of Location.t * string
    (* Lexer Errors *)
    | Unexpected_Character of Location.t * char
    (* Parser Errors *)
    | Unexpected_Token of Location.t * string
    | Unexpected_EndOfInput of Location.t * string
    (* Evaluator/Runtime Errors *)
    | Runtime_Conversion of Location.t * string
    | Unbound_Symbol of Location.t * string
    | Not_Applicable of Location.t * string
    | Arity_Mismatch of Location.t * string * string * int

  let to_string (msg : t) : string =
    match msg with
    | Unreacheable (loc, reason) ->
      Printf.sprintf "%s: unreacheable %s!"
        (Location.to_string loc)
        reason

    | Unexpected_Character (loc, reason) ->
      Printf.sprintf "%s: Unexpected character '%c'"
        (Location.to_string loc)
        reason

    | Unexpected_Token (loc, reason) ->
      Printf.sprintf "%s: Unexpected token %s"
        (Location.to_string loc)
        reason
    
    | Unexpected_EndOfInput (loc, reason) ->
      Printf.sprintf "%s: Unexpected end of input, %s"
        (Location.to_string loc)
        reason
    
    | Runtime_Conversion (loc, reason) ->
      Printf.sprintf "%s: Runtime conversion error: %s"
        (Location.to_string loc)
        reason

    | Unbound_Symbol (loc, reason) ->
      Printf.sprintf "%s: Unbound symbol \"%s\""
        (Location.to_string loc)
        reason
    
    | Not_Applicable (loc, reason) ->
      Printf.sprintf "%s: Not aplicable %s"
        (Location.to_string loc)
        reason

    | Arity_Mismatch (loc, name, args, got) ->
      Printf.sprintf "%s: Arity mismatch, %s %s, but got %d"
        (Location.to_string loc)
        name args got
end

exception Error of Error.t