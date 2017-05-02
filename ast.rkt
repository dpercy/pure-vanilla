#lang racket

(provide (all-defined-out)
         (struct-out Global))

(require "constructor.rkt")

(tag-struct "Syntax" (Module modname statements))

(tag-struct "Syntax" (Using modname))
(tag-struct "Syntax" (Def var expr))

(tag-struct "Syntax" (Lit value))
(tag-struct "Syntax" (Quote ast))

; `number` is not a De-Bruijn index or up-reference:
; it's an extension of the variable's name.
; Two locals are equal iff their `name` and `number` are equal.
(tag-struct "Syntax" (Local name number))

(tag-struct "Syntax" (Func params body))
(tag-struct "Syntax" (Call func args))
(tag-struct "Syntax" (If test consq alt))

(define (Syntax? v)
  (or (Global? v)
      Module?
      Using?
      Def?
      Lit?
      Quote?
      Local?
      Func?
      Call?
      If?))
