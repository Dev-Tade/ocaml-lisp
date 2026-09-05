
module Location = struct
  type t = {
    line:   int;
    column: int;
    source: string;
  }

  type 'a located = 'a * t 

  let make file line column =
    { source = file; line; column; }

  let to_string (loc : t) : string =
    Printf.sprintf "%s:%d:%d" 
      loc.source 
      loc.line 
      loc.column
    
  let print (loc: t) : unit =
    print_string (to_string loc)
end

module Errors = struct
  type t =
    (* General Purpose *)
    | Unreacheable of Location.t * string
    (* Lexer *)
    | Unexpected_Character of Location.t * char
    (* Parser *)
    | Unexpected_Token of Location.t * string
    | Unexpected_EndOfInput of Location.t * string
    (* Evaluator/Runtime *)
    | Invalid_Type of Location.t * string * string * string
    | Invalid_Conversion of Location.t * string * string
    | Unbound_Symbol of Location.t * string
    | Not_Applicable of Location.t * string
    | Arity_Mismatch of Location.t * string * string list * string list

  exception Error of t

  let unreacheable loc where = 
    raise (Error (Unreacheable (loc, where)))
  
  let unexpected_character loc character = 
    raise (Error (Unexpected_Character (loc, character))) 
  
  let unexpected_token loc token_string = 
    raise (Error (Unexpected_Token (loc, token_string)))
  
  let unexpected_eof loc what =
    raise (Error (Unexpected_EndOfInput (loc, what)))
  
  let invalid_type loc typename expected reason =
    raise (Error (Invalid_Type (loc, typename, expected, reason)))
  
  let invalid_conversion loc from_type to_type =
    raise (Error (Invalid_Conversion (loc, from_type, to_type)))

  let unbound_symbol loc symbol = 
    raise (Error (Unbound_Symbol (loc, symbol)))

  let not_applicable loc what =
    raise (Error (Not_Applicable (loc, what)))
    
  let arity_mismatch loc func args got =
    raise (Error (Arity_Mismatch (loc, func, args, got)))

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
    
    | Invalid_Type (loc, typename, expected, reason) ->
      Printf.sprintf "%s: Invalid type %s expected %s: %s"
      (Location.to_string loc) 
      typename expected reason

    | Invalid_Conversion (loc, from_type, to_type) ->
      Printf.sprintf "%s: Invalid conversion, can't convert %s to %s"
        (Location.to_string loc)
        from_type to_type

    | Unbound_Symbol (loc, reason) ->
      Printf.sprintf "%s: Unbound symbol \"%s\""
        (Location.to_string loc)
        reason
    
    | Not_Applicable (loc, reason) ->
      Printf.sprintf "%s: Not aplicable %s"
        (Location.to_string loc)
        reason

    | Arity_Mismatch (loc, name, args, gots) ->
      let suffix = function
      | x when x <> 1 -> "arguments"
      | _ -> "argument"
      in

      let list_suffix args =
        match args with
        | [] -> ""
        | xs -> ": " ^ (String.concat ", " xs)
      in

      let args_count = List.length args in
      let gots_count = List.length gots in
      let miss_count = args_count - gots_count in

      Printf.sprintf "%s: Arity mismatch %s expects %d %s%s, but got %d %s%s. Missing %d %s"
      (Location.to_string loc) name 
      args_count (suffix args_count) (list_suffix args)
      gots_count (suffix gots_count) (list_suffix gots)
      miss_count (suffix miss_count)
end