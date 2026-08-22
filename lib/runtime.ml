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
  | NativeFunction of (metadata -> value list -> value) * metadata
  | SpecialForm of (metadata -> Sexpr.t list -> environment -> value) * metadata
  | Lambda of lambda * metadata
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
    
    | NativeFunction _ ->
      Error (Diagnostics.Error.Runtime_Conversion (rcl, "Can't convert NativeFunction to Sexpr"))

    | SpecialForm _ ->
      Error (Diagnostics.Error.Runtime_Conversion (rcl, "Can't convert SpecialForm to Sexpr"))
      
    | Lambda _ ->
      Error (Diagnostics.Error.Runtime_Conversion (rcl, "Can't convert Lambda to Sexpr"))
      
    | Unit ->
      Error (Diagnostics.Error.Runtime_Conversion (rcl, "Can't convert Unit to Sexpr"))
        
  let rec to_string = function
    | Unit -> "(unit)"
    | Number n -> Printf.sprintf "(number %d)" n 
    | Symbol s -> Printf.sprintf "(symbol %s)" s
    | List xs -> "(list (" ^ String.concat " " (List.map to_string xs) ^ "))"
    | NativeFunction (_, meta) -> "(native "^ Metadata.to_string meta ^ ")"
    | SpecialForm (_, meta) -> "(special-form " ^ Metadata.to_string meta ^ ")"
    | Lambda (func, meta) -> 
      let args = "(" ^ String.concat " " func.params ^ ")" in
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