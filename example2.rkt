#lang vanilla


x = 1

y = 2

mkpair = (left, right) -> selector -> selector(left, right)

p = mkpair(x, y)


mp = (left, right) -> (which -> (if which then left else right))


left = (x, y) -> x
right = (x, y) -> y

z = x Base.+ y


fib = (n) -> if n < 2 then n else fib(n - 1) + fib(n - 2)
