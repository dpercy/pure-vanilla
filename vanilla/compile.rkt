#lang racket

(require racket/syntax)
(require "./parse.rkt")



(define (compile ast) ; -> syntax-object
  (match ast
    [(Program statements) #`(begin #,@(map compile statements))]
    [(Def var expr) #`(define #,(compile var) #,(compile expr))]
    ; all generated identifiers have a dot.
    ; nice side effect: no conflict with Racket ids (quote, lambda, etc).
    [(Local name number)  (format-id #f "~a.~a" name number)]
    [(Global mod name)  (format-id #f "~a.~a" (or mod "") name)]

    [(Lit value)  #`(quote #,value)]
    [(Unresolved name)  (error 'compile "Unresolved identifier: ~s" name)]

    ; TODO also include the raw syntax structs!
    [(Func params body)  #`(lambda #,(map compile params)
                             #,(compile body))]
    [(Call func args)  #`(#%app #,(compile func)
                                #,@(map compile args))]))
