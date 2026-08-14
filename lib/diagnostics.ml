
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