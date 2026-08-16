# Architecture

## Diagnostics
The file [`diagnostics.ml`](lib/diagnostics.ml) defines 2 submodules: `Location` and `Error`. The first module defines a simple record:
```ml
{
  line:   int;
  column: int;
  source: string;
}
```
Used around all stages to give an origin location, it also has convenience functions like `Location.to_string` or `Location.print`

The second module defines an variant type for different kind of errors used across al stages, it also defines a new Exception type for raising these errors. The also module provides `Error.to_string` to convert the errors to readable strings.
All of these errors expect to be built with a tuple:
```ml
(location : Location.t, reason : string)
```
very few errors actually change the reason element to another type, this is because it could generate cyclic dependencies between types. If for example `Error.Unexpected_Token` where to accept `Lexer.Token.t` as reason it would refuse to compile. And the posibility of additional structured field for other errors is not discarded

## Lexer
The current [`lexer.mli`](lib/lexer.mli) publicly exposes a submodule `Token` for representing `lexmes`. Token submodule also exposes `to_string` and `print` functionality.
Otherwise, the main module just exposes a single function: `lex` with the signature:
```ml
open Diagnostics (* For the sake of not writing: Diagnostics.Location.t *)
val lex : string -> source:string -> (Token.t * Location.t) list
```
The first string argument is the actual source code to produce tokens from. The second and named argument `source` is the source of that code, primarily used for `Diagnostics.Location.t.source` record field. For example on the REPL `~source:"<repl>"` otherwise it could be the file name like `~source:"main.lisp"`.

Notice how the return type of the function is `(Token.t * Location.t) list`, so a list of tuples containing a `Token` and its `Location`. In the case of token was simpler to not make the location a member of the core type and just return a pair.

The actual implementation [`lexer.ml`](lib/lexer.ml) implements all the publicly exposed functionality as well as helper functions and a core `Lexer.t` representing the lexer state. The actual lexing process is a recursive function:
```ml
let rec next_token (state : Lexer.t) : (Token.t * Location.t) list
```
It takes the current lexer state and produces a list of tokens, the process, it lexes from left to right and each time recursively calling itself to process the next token.
Finally `lex` function just calls `next_token` with an initial state, thus recursively parsing the entire source code.

It could be a good idea to expose a variant of this function publicly, as well as a dumb init function for private `Lexer.t`. So the parser could ask for tokens as it needs them rather than receiving the entire lexed source code in form of a list. This would imply that the parser holds a lexer state internally. Not completely assured but it theoretically could increase performance by reducing memory consumption (eliminating the need of the whole token list being processed beforehand), and early failure because if a parser error occurs it wouldn't have waited the entire lexing process to finish as it would be doing it on demand.

## Parser
The [`parser.mli`](lib/parser.mli) publicly exposes a submodule `Sexpr` that holds the AST representation of the expressions:
```ml
type node =
  | Symbol of string
  | Number of int
  | List of t list
and t = node Location.located
```
The type is quite complex, more than it should probably. The core of a `Sexpr` (`S`ymbolic `expr`ession) is that is either an `atom` (in this case an atom makes for a `Symbol` containing a string, or a `Number` containing a int) or a `list` where this `List` is recursively a symbolic expression. In the case of `List` is of type `t list` where `t` is a mutually recursive type (defined at the `and` clause) because each `Sexpr`, regardless of being an atom or a list, its `located` (`Location.located` is just a shorthand to writing `'a * Location.t`).
This allows for any expression to have an origin location, which is useful when generating error diagnostics.

This submodule also exposes `to_string` and `print` as usual. But it also exposes `raw` and `location` functions, these become really useful when pattern matching. 
Because sometimes you don't care about the actual location, and just want the raw node (for example in the `to_string` function):
```ml
let rec aux (expr : t) : string =
  match raw expr with
    | Symbol s -> "(Symbol " ^ s ^ ")"
    | Number n -> "(Number " ^ string_of_int n ^ ")"
    | List xs -> "(" ^ String.concat "" (List.map aux xs) ^ ")"
```
If the raw weren't used there it would've been written as `match (expr, _) with ...`

Finally the main `Parser` module just exposes:
```ml
(* Just to not write Lexer.Token.t and Diagnostics.Location.t *)
open Diagnostics
open Lexer

val parse : (Token.t * Location.t) list -> Sexpr.t list
```

The implementation [`parser.ml`](lib/parser.ml) similar to the lexer implementation defines a core type `parserState` which holds parser state:
```ml
type parserState = 
{
  current : Sexpr.t list;
  stack : (Sexpr.t list * Location.t) list;
  last_loc : Location.t option
}
```
The fields on this record works as following, the `current` is the current expression being parsed (notice its `Sexpr.t list` because it contains the expressions accumulated inside the current list). The `stack` field is a stack of pairs where a pair is `(Sexpr.t list * Location.t)` the first element on the pair is the `previous list` and the second element is the `start location of the current list` the stack is modeled as a list. Finally the `last_loc` field is the last successfully consumed token location, it is useful when reaching an edge case such as `missing closing parenthesis`.

Finally the actual `parse` function is similar to the `lex` one in the sense that is has an internal `next_token` recursive helper in charge of actually doing the parsing.
I'll not explain the parsing algorithm, as I believe the actual code is straight forward enough to understand.


## Runtime
The [`runtime.mli`](lib/runtime.mli) module is the clear frontier between the runtime values and the parser representation. It also defines core constructs like `environment` containing symbol bindings. It is mainly divided into 3 submodules, notice how regardless of these submodules, the core types are defined at top level `Runtime` module (because of mutually recursivity and visibility):
#### Metadata
The metadata submodule defines a record whose fields should always be optional since not all values may actually need the same metadata, but metadata should always be associated to runtime values similar to how a `Location.t` is always tied to `Sexpr.t`.
As usual conveinience functions `to_string` and `print` as well simple builders like `none` for making an empty metadata record and `make` for avoiding curly braces syntax.
#### Value
The actual runtime value, its a conversion from `Sexpr.t` to a similar variant type, but it has a few more special variants: `NativeFunction`, `SpecialForm` and `Lambda` these are values representing runtime functions, the difference between them is where they are implemented and what do they expect:
* `NativeFunction`: Is an OCaml implemented function that accepts `value list` and produces `value`
* `SpecialForm`: Is an OCaml implemented function that expects `Sexpr.t list` and produces `value` the difference here is that there is no evaluation of arguments being passed to the function, this is because as the name implies they are special forms. Such as `if` where if the arguments were evaluated beforehand it couldn't serve the purpose of discriminating `then` and `else` clause.
* `Lambda`: Is a runtime function, similar to `Nativefunction` it takes `value list` and produces `value` but its implemented differently as the variant holds the `closure` (environment where the function was defined), `body` (the actual `Sexpr.t` code representation of the function) and `params` (a list of strings used to bind arguments to function parameter names)

Now an important distinction is how `NativeFunction` and `SpecialForm` implementation are defined:
```ml
| NativeFunction of (metadata -> value list -> value) * metadata
| SpecialForm of (metadata -> Sexpr.t list -> environment -> value) * metadata
```
Both store metadata alongside their implementation (`* metadata`) this is so called "definition metadata", basically in which `.ml` file, at which line and which name it has. And the implementation function of these callables also accepts metadata but this one is so called "use metadata", basically where they were used in runtime, useful if there is an error inside the implementation function. The "use metadata" is typically derived from "definition metadata", but normally with the location being where the callable was used:
```lisp
; main.lisp
(if (> 2 x)) ; missing <then> 
```
The "use metadata" would be useful for the function to produce something like "main.lisp:1:1: Missing then part for special form if".
As usual convenience functions `to_string` and `print` exist for value.

#### Environment
The environment holds the current symbol bindings at runtime, it provides meaning to functions for example: `print` comes out from `Parser.parse` as `Sexpr.Symbol` but when evaluating it ends up as `Runtime.NativeFunction` (assuming its available at the base runtime environment, otherwise will raise something like `Unbound Symbol ...`)

Its submodule defines functions `create`, `lookup`, `define` in order to work with environments:
* `create`: `parent`, Creates a new environment optionally with a parent one.
* `lookup`: `env name`, Searches for binding with name = `name` at environment `env`. It recursively searches on the parents bindings when linked with some parent.
* `define`: `env name value`, Defines a binding with name = `name` at environment `env`, with value = `value`.

---

`Runtime.value` exists because it represents values that exist during evaluation. Although Symbol, Number and List have corresponding representations in `Sexpr.t`, they differ semantically: `Sexpr.t` represents source expressions, while `Runtime.value` represents evaluated runtime data.

Finally implementation [`runtime.ml`](lib/runtime.ml) just holds implementation for all the publicly defined functionality