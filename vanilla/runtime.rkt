#lang racket

(provide (all-defined-out))

(require (submod "parse.rkt" ast))

(struct Function (procedure syntax)
  ; TODO put a syntax-thunk here instead, to avoid constructing all the syntax nodes
  ; when they're not needed.
  #:property prop:procedure
  (struct-field-index procedure))
