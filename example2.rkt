#lang vanilla


x = 1

y = 2

mkpair = (left, right) -> selector -> selector(left, right)

p = mkpair(x, y)


mp = (left, right) -> (which -> (if which then left else right))


left = (x, y) -> x
right = (x, y) -> y
