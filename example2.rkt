#lang vanilla


x = 1

y = 2

mkpair = (left, right) -> selector -> selector(left, right)

p = mkpair(x, y)
