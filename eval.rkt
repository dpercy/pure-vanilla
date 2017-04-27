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
                                  (error 'eval "module ~a doesn't define ~a" mn name)))))])
       ,core-expr))
  ;(displayln (list 'compiled core-expr))
  (racket:eval (datum->syntax #'() wrapped-expr)))

(define (compile expr)
  (let recur ([expr expr]
              [current-mod #f])
    (define (compile expr) (recur expr current-mod))
    (match expr
      [(Module mn body) (let* ([globals (map Def-var (filter Def? body))]
                               [global-names (map Global-name globals)]
                               [global-exprs (for/list ([g globals]) (recur g mn))])
                          `(let ()
                             ,@(for/list ([stmt body])
                                 (recur stmt mn))
                             ; a run-time construct a hash wrapped in a Mod.
                             (Mod (quote ,mn)
                                  (for/hash ([name '(,@global-names)]
                                             [value (list ,@global-exprs)])
                                    (values name value)))))]
      [(Def lhs rhs) `(define ,(compile lhs) ,(compile rhs))]
      [(? Using?) `(void)]
      ; expressions
      [(Lit v) `(quote ,v)]
      [(Quote ast) `(quote ,ast)]
      [(Local name num) (format-symbol "~a.~a" name num)]
      [(Global mod name) (if (equal? mod current-mod)
                             (format-symbol "~a.~a" mod name)
                             `(get-global (quote ,mod) (quote ,name)))]
      [(Func params body) `(lambda ,(map compile params)
                             ,(compile body))]
      [(Call func args) `(#%app ,(compile func)
                                ,@(map compile args))]
      [(If t c a) `(if ,(compile t)
                       ,(compile c)
                       ,(compile a))])))
