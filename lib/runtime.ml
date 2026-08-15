open Diagnostics
open Parser

type metadata = 
{
  name : string option;
  location : Location.t option;
}

type value =
  | Unit
  | Number of int
  | Symbol of string
  | List of value list
  | NativeFunction of metadata * (metadata -> value list -> value)
  | SpecialForm of metadata * (metadata -> Sexpr.t list -> environment -> value)
  | Lambda of metadata * lambda
and lambda = 
{
  closure: environment;
  params: string list;
  body: Sexpr.t;
}
and environment =
{ 
  values : (string, value) Hashtbl.t;
  parent : environment option;
}
module Metadata = struct
  type t = metadata
  let none = { name = None; location = None; }
  
  let make name file line column =
    { name = Some name; location = Some { source = file; line; column = column; } }

  let to_string metadata =
    let name = 
      match metadata.name with
      | None -> "anonymous"
      | Some n -> n
    in

    let location = 
      match metadata.location with
      | None -> ""
      | Some l -> " @ " ^ Location.to_string l
    in

    name ^ location

  let print metadata =
    print_string (to_string metadata)
end
module Value = struct
  type t = value
  
  let rec to_string = function
    | Unit -> "(unit)"
    | Number n -> Printf.sprintf "(number %d)" n 
    | Symbol s -> Printf.sprintf "(number %s)" s 
    | List xs -> "(list (" ^ String.concat " " (List.map to_string xs) ^ "))"
    | NativeFunction (meta, _) -> "(native "^ Metadata.to_string meta ^ ")"
    | SpecialForm (meta, _) -> "(special-form " ^ Metadata.to_string meta ^ ")"
    | Lambda (meta, func) -> 
      let args =
        "(" ^ String.concat " " func.params ^ ")"
      in
    
      "(lambda " ^ Metadata.to_string meta ^ " " ^ args ^ ")"


  let print (v : t) : unit = 
    print_string (to_string v);
    ()
end

module Environment = struct
  type t = environment
  
  let create parent = { values = Hashtbl.create 16; parent; }
  let define env name value = Hashtbl.replace env.values name value

  let rec lookup env name =
    (* Search name in current environment *)
    match Hashtbl.find_opt env.values name with
    | Some value -> Some value
    | None ->
      (* Search name in parent environment if any *)
      match env.parent with
      | Some parent -> lookup parent name
      | None -> None
end