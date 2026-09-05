open Diagnostics
open Parser

type arity =
| Fixed of int
| Range of int * int
| AtLeast of int

type applicable =
{
  arity : arity;
  args  : string list;
}

type metadata =
{
  name : string option;
  location : Diagnostics.Location.t option;
}

type value =
| Unit
| Number of int
| Symbol of string
| List of value list
| Native of native
| SpecialForm of special_form
| Lambda of lambda
and native =
{
  applicable : applicable;
  metadata   : metadata;
  body       : metadata -> value list -> environment -> value;
}
and special_form =
{
  applicable : applicable;
  metadata   : metadata;
  body       : metadata -> Sexpr.t list -> environment -> value;
}
and lambda = 
{
  applicable : applicable;
  metadata   : metadata;
  closure    : environment;
  body       : Sexpr.t;
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

  let fill_w_fallback meta fall =
    {
      name =
        begin match meta.name with
        | Some _ -> meta.name
        | None ->   fall.name
        end;

      location =
        begin match meta.location with
        | Some _ -> meta.location
        | None ->   fall.location
        end;
    }

  let name_or meta name =
    match meta.name with
    | Some n -> n
    | None   -> name
  
  let location_or meta loc =
    match meta.location with
    | Some l -> l
    | None   -> loc

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

module Applicable = struct
  type t = applicable

  let fixed args = 
    { 
      arity = Fixed (List.length args);
      args  = args 
    }

  let range min args = 
    let max = List.length args in

    if min < 0 then 
      invalid_arg "Applicable.range: minimum cannot be negative";
    if min > max then 
      invalid_arg "Applicable.range: minimum cannot exceed maximum";

    { 
      arity = 
        if min = max then Fixed max 
        else Range (min, max);
      args  = args
    }
  
  let at_least args =
    let min = List.length args in
    
    if min < 0 then
    invalid_arg "Applicable.at_least minimum cannot be negative";

    {
      arity = AtLeast min;
      args  = args;
    }

  let check arity input =
    match arity with
    | Fixed count      -> input = count
    | Range (min, max) -> input >= min && input <= max
    | AtLeast min      -> input >= min

  let to_string applicable =
    let suffix c = if c = 1 then "argument" else "arguments" in

    match applicable.arity with
    | Fixed count -> 
      Printf.sprintf "expects %d %s: %s"
        count
        (suffix count)
        (String.concat ", " applicable.args)

    | Range (min, max) ->
      Printf.sprintf "expects between %d and %d %s: %s"
        min max
        (suffix max)
        (String.concat ", " applicable.args)

    | AtLeast min ->
      Printf.sprintf "expects at least %d %s: %s"
        min
        (suffix min)
        (String.concat ", " applicable.args)
  
  let print arity =
    print_string (to_string arity)
end

module Value = struct
  type t = value

  let rec from_sexpr sexpr =
    match Sexpr.raw sexpr with
    | Sexpr.Number n -> Number n
    | Sexpr.Symbol s -> Symbol s
    | Sexpr.List xs ->
      List ( List.map from_sexpr xs )

  let rec to_sexpr value =
    (* 
      Dummy location for Sexpr.t coming from runtime conversions,
      if at some point the other runtime value variants get metadata
      then this code should be updated to use that instead.
      rcl = runtime conversion location 
    *)
    let rcl : Location.t = 
      { source = __FILE__; line = __LINE__; column = 0; }
    in

    let rec map_result f xs =
      List.fold_right
      (fun x acc ->
        match f x, acc with
        | Result.Ok y, Result.Ok ys -> Result.ok (y :: ys)  
        | Result.Error err, _ -> Result.Error err  
        | _, Result.Error err -> Result.Error err)
      xs (Result.Ok [])
    in

    match value with
    | Number n -> Ok (Sexpr.Number n, rcl)
    | Symbol s -> Ok (Sexpr.Symbol s, rcl)
    | List xs -> (
      match map_result to_sexpr xs with
      | Error err -> Error err
      | Ok xss -> Ok (Sexpr.List xss, rcl)
    )
    
    | Native _ -> Error (rcl, "Native")

    | SpecialForm _ -> Error (rcl, "SpecialForm")
      
    | Lambda _ -> Error (rcl, "Lambda")
      
    | Unit ->
      Error (rcl, "Unit")

  let basetype = "Value"

  let typename = function
  | Unit          -> "unit"
  | List _        -> "list"
  | Number _      -> "number"
  | Symbol _      -> "symbol"
  | Native _      -> "native"
  | Lambda _      -> "lambda"
  | SpecialForm _ -> "special-form"
        
  let rec to_string t =
    match t with
    | Unit -> "(" ^ typename t ^ ")"
    | Number n -> "(" ^ typename t ^ " " ^ string_of_int n ^ ")" 
    | Symbol s -> "(" ^ typename t ^ " " ^ s ^ ")" 

    | List xs -> 
      "(" ^ typename t ^ " (" ^ String.concat " " (List.map to_string xs) ^ "))"

    | Native native -> 
      "(" ^ typename t ^ " " ^ Metadata.to_string native.metadata ^ ")"
      
    | SpecialForm sform -> 
      "(" ^ typename t ^ " " ^ Metadata.to_string sform.metadata ^ ")"
      
    | Lambda lambda -> 
      let args = "(" ^ String.concat " " lambda.applicable.args ^ ")" in
      let meta = Metadata.to_string lambda.metadata in
      "(" ^ typename t ^ " " ^ meta ^ " : " ^ args ^ ")"

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