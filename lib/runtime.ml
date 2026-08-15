open Parser

type value =
  | Unit
  | Number of int
  | Symbol of string
  | List of value list
  | NativeFunction of (value list -> value)
  | SpecialForm of (Sexpr.t list -> environment -> value)
  | Function of {
    params: string list;
    body: Sexpr.t;
    env: environment
  }
and environment =
{ 
  values : (string, value) Hashtbl.t;
  parent : environment option;
}
module Value = struct
  type t = value
  
  let rec to_string = function
    | Unit -> "(unit)"
    | Number n -> Printf.sprintf "(number %d)" n 
    | Symbol s -> Printf.sprintf "(number %s)" s 
    | List xs -> "(list (" ^ String.concat " " (List.map to_string xs) ^ "))"
    | NativeFunction _ -> "(native-function)"
    | SpecialForm _ -> "(special-form)"
    | Function _ -> "(function)"

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