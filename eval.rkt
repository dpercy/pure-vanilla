#lang racket

(provide eval (struct-out Mod))

#|

Limitations of this simple evaluator:
- modules must come in dependency order
- statements must come in dependency order
- re-definitions are not checked/prevented
- adding to an existing module is not checked/prevented

|#

(require (prefix-in racket: racket)
         (only-in racket/syntax format-symbol)
         "ast.rkt")

(struct Mod (name values) #:transparent)


(define (eval ast modstore)
  (define core-expr (compile ast))
  (define wrapped-expr
    `(let ([get-global
            (quote ,(lambda (mn name)
                      (hash-ref (Mod-values
                                 (hash-ref modstore mn
                                           (lambda ()
                                             (error 'eval "no such module: ~a" mn))))
                                name
                                (lambda ()
                                  (error 'eval "module ~a doesn't define ~a" mn name)))))]
           [set-global!
            (quote ,(lambda (mn name value)
                      (match-define (Mod _ values)
                        (hash-ref modstore mn
                                  (lambda () (Mod mn (hash)))))
                      (hash-set! modstore mn
                                 (Mod name (hash-set values name value)))))])
       ,core-expr))
  (displayln (list 'compiled core-expr))
  (racket:eval (datum->syntax #'() wrapped-expr)))

(define (compile expr)
  (match expr
    [(Module mn body) `(begin ,@(map compile body) (void))]
    [(Def (Global mn name) rhs) `(set-global! (quote ,mn) (quote ,name) ,(compile rhs))]
    [(? Using?) `(void)]
    ; expressions
    [(Lit v) `(quote ,v)]
    [(Quote ast) `(quote ,ast)]
    [(Local name num) (format-symbol "~a.~a" name num)]
    [(Global mod name) `(get-global (quote ,mod) (quote ,name))]
    [(Func params body) `(lambda ,(map compile params)
                           ,(compile body))]
    [(Call func args) `(#%app ,(compile func)
                              ,@(map compile args))]
    [(If t c a) `(if ,(compile t)
                     ,(compile c)
                     ,(compile a))]))
