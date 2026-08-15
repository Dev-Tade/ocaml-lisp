
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

module Msg = struct
  type t =
    | Unexpected_Token of Location.t * string
    | Unexpected_EndOfInput of Location.t * string

  let to_string (msg : t) : string =
    match msg with
    | Unexpected_Token (loc, reason) ->
      Printf.sprintf "%s: Unexpected token: %s"
        (Location.to_string loc)
        reason
    
    | Unexpected_EndOfInput (loc, reason) ->
      Printf.sprintf "%s: Unexpected end of input: %s"
        (Location.to_string loc)
        reason
end

exception Message of Msg.t