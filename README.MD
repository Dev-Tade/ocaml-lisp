# OCaml-Lisp

A "Lisp Interpreter" (Not a complete "Lisp", its a barely minimal subset). Made in a weekend with OCaml, being my first "project" with functional programming.
Being my first time with a new language and paradigm don't expect good code quality. There is a lot of improvements to be made, I don't know if im going to keep working on it.

### The Subset:
  * Numbers (**integers only**), Lists
  * `if <cond> <success> <failure>`
  * `def <name> <value>`
  * `lambda (<params>) <body>`
  * `+ (add)` and `* (multiply)` operations for **Numbers only**

### Try it:
Run `dune exec ocaml-lisp` to enter in a repl.
Beware its full of errors, for example:
```lisp
ocaml-lisp> ((lambda (x) (x)) 2 4)
Fatal error: exception Invalid_argument("List.iter2")
```
This crashes out the repl, The problem here is that there are more arguments being applied than the arity of the lambda, however because of poor code quality and structure this case isn't managed and thus crashes.

Some other examples (that do work as "expected"):
```lisp
ocaml-lisp> def x h                 
# (special-form)
! Error: Unbound symbol: x
```
Here the error is that `x` is unbound at this point, and `def` isn't being applied (to be applied it requires to be the operand of a list): `(op arg arg arg ...)`
A correct definition could be something like:
```lisp
ocaml-lisp> (def x 1)
# (unit)
ocaml-lisp> x
# (number 1)
```

```lisp
ocaml-lisp> print
# (native-function)
ocaml-lisp> (print 2)
(number 2)
# (unit)
ocaml-lisp> 2
# (number 2)
```
Notice how here `(number 2)` appears twice, when there's a `#` preceding the output is the REPL output, otherwise is stdout/print.

Lets define a function
```lisp
ocaml-lisp> (lambda (x) x)
# (lambda)
```
Lets apply something to it
```lisp
ocaml-lisp> ((lambda (x) x) 2)
# (number 2)
```
Simple identity function isn't it?

Functions require the parameter section to be a list:
```lisp
ocaml-lisp> (lambda a a)
! Error: Function expects parameters (list) and body
ocaml-lisp> (lambda (a) a)
# (lambda)
ocaml-lisp> (lambda (a))
! Error: Function expects parameters (list) and body
```
Notice how again because of poor code quality and structure, it can't tell you what's exactly missing. In the second error it should tell you that the body is missing.

Now this isn't useful at all, save it to a binding so its useful:
```lisp
ocaml-lisp> (def identity (lambda (a) a))
# (unit)
ocaml-lisp> identity
# (lambda)
ocaml-lisp> (identity 2)
# (number 2)
```

Finally I'll show you some if statements:
```lisp
ocaml-lisp> if
# (special-form)
ocaml-lisp> (def a 1)
# (unit)
ocaml-lisp> (if a (* a 2) a)
# (number 2)
ocaml-lisp> (def a 0)
# (unit)
ocaml-lisp> (if a (* a 2) a)
# (number 0)
```

### TBD?
There is a single TODO documented in the code. But in reality there are many more things to be done.
For example: adding `quote` support, better code structure and organization, error handling, and many more.