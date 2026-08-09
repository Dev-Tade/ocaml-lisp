
type value =
  | Unit
  | Number of int
  | Symbol of string
  | List of value list
  | NativeFunction of (value list -> value)
  | SpecialForm of (Sexpr.expr list -> env -> value)
  | Function of {
    params: string list;
    body: Sexpr.expr;
    env: env
  }


and env = 
{
  values : (string, value) Hashtbl.t;
  parent : env option;
}

let create parent =
  {
    values = Hashtbl.create 16;
    parent;
  }

let define env name value : unit =
  Hashtbl.replace env.values name value 

let rec lookup env name : value option =
  match Hashtbl.find_opt env.values name with
  | Some value -> Some value
  | None ->
    match env.parent with
    | Some parent -> lookup parent name
    | None -> None

let rec value_to_string value =
  match value with
  | Unit -> "(unit)"
  | Number n -> Printf.sprintf "(number %d)" n 
  | Symbol s -> Printf.sprintf "(number %s)" s 
  | List _ -> "(list)"
  | NativeFunction _ -> "(native-function)"
  | SpecialForm _ -> "(special-form)"
  | Function _ -> "(function)"
;;

let print_value value =
  print_endline (value_to_string value)
;;