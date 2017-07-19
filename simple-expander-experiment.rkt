#lang racket


#|

Idea:
- simple expression expander
- custom set of primitive forms
- check each ID as a prim before desugaring

core:
x
(lambda (x ...) e)
(e e ...)
(if e e e)
(quote v)



|#

(define (atom? v)
  (and (not (symbol? v))
       (not (empty? v))
       (not (cons? v))))

(define (expand form)
  (match form
    [(? atom?) (expand `(quote ,form))]
    [(? symbol?) form]
    ['() (error 'expand "empty parens not allowed")]
    [(cons head args)
     (match head
       ; check for primitive meaning
       ['quote (expand-prim-quote form)]
       ['lambda (expand-prim-lambda form)]
       ['if (expand-prim-if form)]
       ; check for macro meaning
       ['and (expand-macro-and form)]
       ['let (expand-macro-let form)]
       ; treat as application otherwise
       [_ (map expand form)])]))

; primitives call expand on each subform
(define (expand-prim-quote form)
  (match form
    [(list 'quote v) form]))
(define (expand-prim-lambda form)
  (match form
    [(list 'lambda (list (? symbol? args) ...) body)
     `(lambda (,@args) ,(expand body))]))
(define (expand-prim-if form)
  (match form
    [(list 'if test consq alt)
     `(if ,(expand test) ,(expand consq) ,(expand alt))]))

; macros call expand on the entire result
(define (expand-macro-and form)
  (expand
   (match form
     [(list 'and) #true]
     [(list 'and x y) `(if ,x ,y #false)]
     [(list* 'and x rest) `(and x (and ,@rest))])))
(define (expand-macro-let form)
  (expand
   (match form
     [(list 'let (list (list lhs rhs) ...) body)
      `((lambda (,@lhs) ,body) ,@rhs)])))

(module+ test
  (require rackunit)

  (check-equal? (expand '(let ([x 1]) (g (and x (f 4)))))
                '((lambda (x) (g (if x (f '4) '#false)))
                  '1)))
