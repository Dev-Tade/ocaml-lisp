
type expr =
  | Symbol of string
  | Number of int
  | List of expr list

let print_expr expr =
  let rec aux indent expr =
    match expr with
    | Symbol s -> 
      print_endline (indent ^ "Symbol: " ^ s)

    | Number n -> 
      print_endline (indent ^ "Number: " ^ string_of_int n)

    | List xs ->
      print_endline (indent ^ "List: ");
      List.iter (aux (indent ^ "  ")) xs
  in
  aux "" expr

let is_number =
  function
  | Number _ -> true
  | _ -> false

let is_symbol =
  function
  | Symbol _ -> true
  | _ -> false

let is_list =
  function
  | List _ -> true
  | _ -> false

let rec count predicate expr =
  let current =
    if predicate expr then 1 else 0
  in

  match expr with
  | List xs ->
    List.fold_left
    (fun acc expr -> acc + count predicate expr)
    current
    xs

  | _ ->
    current
