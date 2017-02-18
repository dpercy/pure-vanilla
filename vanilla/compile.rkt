#lang racket

(provide compile)

(require racket/syntax)
(require (submod "./parse.rkt" ast))
(require (for-template "./runtime.rkt"))


(define (compile ast) ; -> syntax-object or list of syntax-object
  (match ast
    [(Program statements) (map compile statements)]
    ; TODO see ./macro-demos for how to do the correct evaluation order
    [(Def var expr) #`(begin
                        (provide (rename-out [#,(compile var)
                                              #,(Global-name var)]))
                        (define #,(compile var) #,(compile expr)))]
    ; all generated identifiers have a dot.
    ; nice side effect: no conflict with Racket ids (quote, lambda, etc).
    [(Local name number)  (format-id #f "~a.~a" name number)]
    [(Global mod name)  (format-id #f "~a.~a" (or mod "") name)]

    [(Lit value)  #`(quote #,value)]
    [(Unresolved name)  (error 'compile "Unresolved identifier: ~s" name)]

    [(Func params body)  #`(Function (lambda #,(map compile params)
                                       #,(compile body))
                                     #,(compile-template ast (set)))]
    [(Call func args)  #`(#%app #,(compile func)
                                #,@(map compile args))]
    [(If test consq alt) #`(if (boolean=? #true #,(compile test))
                               #,(compile consq)
                               #,(compile alt))]))

; compile-template "quotes" the ast:
; it produces a syntax-object that when evaluated returns that AST.
; However, any free local variables in the AST are compiled to Racket variables.
; This allows currying like (Function-syntax "let x = 1 in () -> f(x)")
; to return "() -> f(1)".
(define (compile-template ast env) ; -> syntax-object
  ; env : set( Local )
  (match ast
    [(Local name number)  (if (set-member? env ast)
                              #`(quote #,ast)
                              #`(value->syntax #,(format-id #f "~a.~a" name number)))]
    [(Global mod name)  #`(quote #,ast)]

    [(Lit value)  #`(quote #,ast)]
    [(Unresolved name)  (error 'compile "Unresolved identifier: ~s" name)]

    [(Func params body)  (let ([env (for/fold ([env env]) ([p params])
                                      (set-add env p))])
                           #`(Func (list #,@(for/list ([p params])
                                              (compile-template p env)))
                                   #,(compile-template body env)))]
    [(Call func args)  #`(Call #,(compile-template func env)
                               (list #,@(for/list ([a args])
                                          (compile-template a env))))]
    [(If test consq alt)  #`(If #,(compile-template test  env)
                                #,(compile-template consq env)
                                #,(compile-template alt   env))]))
