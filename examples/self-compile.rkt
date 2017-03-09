#lang vanilla

using Prelude

# syntax helper
cases = (v, handlers) ->
if isEmpty(handlers) then error("no case")
else let h0 = first(handlers) in
let tag = first(h0) in
let f = first(rest(h0)) in
let maybeContents = unpack(tag, v) in
if maybeContents != false then apply(f, maybeContents)
else cases(v, rest(handlers))

addBetween = (lst, sep) ->
if isEmpty(lst) then lst
else if isEmpty(rest(lst)) then lst
else cons(first(lst), cons(sep, addBetween(rest(lst), sep)))
assert(==, addBetween([1,2,3], 0), [1,0,2,0,3])

commaSep = lst -> apply(strcat, addBetween(lst, ", "))

# simple js math compiler
compile = expr ->
if isFunc(expr) then compile(inspect(expr))
else cases(expr, [
                  [:Syntax.Func,
                   (params, body) ->
                   parens(strcat(
                                 parens(commaSep(map(compile, params))),
                                       " => ",
                                       compile(body)))],
                  [:Syntax.Local, (name, num) -> strcat(name, "_", show(num))],
                  [:Syntax.Lit, show],
                  [:Syntax.Call, (func, args) ->
                                 cases(func, [
                                              [:Syntax.Global, (mod, name) ->
                                                               # TODO shouldn't x == :Base.+ work ??
                                                               if [mod, name] == ["Base", "+"]
                                                               then parens(apply(strcat, addBetween(map(compile, args), " + ")))
                                                               else strcat(compile(func), parens(commaSep(map(compile, args))))
                                                               ]
                                              ])]
                  ])

parens = s -> strcat("(", s, ")")

assert(==, compile(x -> x), "((x_0) => x_0)")
assert(==, compile(() -> 1), "(() => 1)")
assert(==, compile((x, y) -> y), "((x_0, y_0) => y_0)")
assert(==, compile(x -> x + 1), "((x_0) => (x_0 + 1))")
# TODO more math operators
