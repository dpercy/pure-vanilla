#lang racket


(provide (all-defined-out))

(struct Syntax () #:prefab)

(struct Module Syntax (modname statements) #:prefab)

(struct Using Syntax (modname) #:prefab)
(struct Def Syntax (var expr) #:prefab)

(struct Lit Syntax (value) #:prefab)
(struct Quote Syntax (ast) #:prefab)

; `number` is not a De-Bruijn index or up-reference:
; it's an extension of the variable's name.
; Two locals are equal iff their `name` and `number` are equal.
(struct Local Syntax (name number) #:prefab)
(struct Global Syntax (mod name) #:prefab)

(struct Func Syntax (params body) #:prefab)
(struct Call Syntax (func args) #:prefab)
(struct If Syntax (test consq alt) #:prefab)
