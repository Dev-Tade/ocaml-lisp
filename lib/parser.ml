open Lexer
open Diagnostics

module Sexpr = struct
  type node =
    | Symbol of string
    | Number of int
    | List of t list
  and t = node Location.located

  let raw (expr, _) = expr
  let location (_, loc) = loc

  let make_symbol sym loc = (Symbol sym, loc)
  let make_number num loc = (Number num, loc)
  let make_list items loc = (List items, loc)

  let to_string (expr : t) =
    let rec aux expr =
      match raw expr with
      | Symbol s -> "(Symbol " ^ s ^ ")"
      | Number n -> "(Number " ^ string_of_int n ^ ")"
      | List xs -> "(" ^ String.concat "" (List.map aux xs) ^ ")"
    in
    aux expr

  let print ?(locate = false) (expr : t) =
    let rec aux indent expr =
      let prefix = 
        if locate then Location.to_string (location expr) ^ ": "
        else ""
      in
      
      match raw expr with
      | List xs ->
        print_endline (prefix ^ indent ^ "(");
        List.iter (aux (indent ^ "  ")) xs;
        print_endline (prefix ^ indent ^ ")")

      | _ -> print_endline (prefix ^ indent ^ to_string expr)
    in
    aux "" expr
end

type parserState = 
{
  current : Sexpr.t list;
  stack : (Sexpr.t list * Location.t) list;
  last_loc : Location.t option
}

let parse (tokens : (Token.t * Location.t) list) : Sexpr.t list =
  let rec next_token tokens state =
    match tokens with
    (* No more tokens *)
    | [] -> ( 
      match state.stack with
      (* All lists are closed *)
      | [] ->
        (* Return reversed list of all parsed expressions *)
        List.rev state.current
      
      (* Missing closing parenthesis *)
      | _ ->
        let eoi_loc =
          match state.last_loc with
          | Some l -> l
          | None -> failwith "How did you reach this?"
        in
        Errors.unexpected_eof eoi_loc "Missing ')'"
    )

    | now :: rest ->
      let tok, loc = now in
      
      match tok with
      (* Illegal Token *) 
      | Token.Illegal -> 
        Errors.unexpected_token loc (Token.to_string tok)
      
      (* List opening *)
      | Token.LParen ->
        (* 
          Push to stack current list being parsed 
          create a new empty current 
        *)
        next_token rest { 
          stack = (state.current, loc) :: state.stack;
          current = [];
          last_loc = Some loc; 
        }
  
      | Token.RParen -> (
        (* 
          Match against stack state, having no items means
          trying to close a non existent list, otherwise
          close current one being parsed
        *)
        match state.stack with
        (* Close a non existent list *)
        | [] ->
          Errors.unexpected_token loc (Token.to_string tok)

        (* Close the current list being parsed *)
        | (parent_list, opening_loc) :: rest_stack -> 
          (* 
            Reverse the order of list elements
            as parser goes from right to left 
          *)
          let closed = (Sexpr.List (List.rev state.current), opening_loc) in

          next_token rest { 
            current = closed :: parent_list;
            stack = rest_stack;
            last_loc = Some loc
          }
      )
      
      | Token.Number n -> 
        (* 
          Push to current the number just parsed
        *)
        next_token rest { 
          state with
          current = Sexpr.make_number n loc :: state.current;
          last_loc = Some loc
        }
      
      | Token.Symbol s -> 
        (* 
          Push to current the symbol just parsed
        *)
        next_token rest {
          state with
          current = Sexpr.make_symbol s loc :: state.current;
          last_loc = Some loc
        }
  in 
  next_token tokens { current = []; stack = []; last_loc = None }
;;