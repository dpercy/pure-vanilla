#lang vanilla


f = () -> :asdf
g = (asdf) -> () -> [asdf, :asdf]



cases = (v, handlers) ->
  if isEmpty(handlers) then error("no case")
  else let h0 = first(handlers) in
       let tag = first(h0) in
       let f = first(rest(h0)) in
       let maybeContents = unpack(tag, v) in
       if maybeContents != false then apply(f, maybeContents)
       else cases(v, rest(handlers))

funcParams = (v) -> cases(v, [
  [:Syntax.Func, (params, body) -> params]
])                             
funcBody = (v) -> cases(v, [
  [:Syntax.Func, (params, body) -> body]
])                             

unfunc = (v) -> unpack(:Syntax.Func, v)

fc = unpack(:Syntax.Func, inspect(f))
gc = unpack(:Syntax.Func, inspect(g))
