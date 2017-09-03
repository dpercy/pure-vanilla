#lang racket


#|

Let's take the idea from ccc2.rkt and turn it into a rewriting system.
Instead of modeling just "flat function" values, we'll model all expressions.
And we'll have a step function that rewrites one expression to another.

What's neat is these expressions will not have Lambda or scope!
This makes it easy to conflate values and syntax.

|#
(module+ test (require rackunit))



#|

expr :=

; literals
(quote v)

; application
(call E E)

; built-in functions and combinators
const
id
compose
pair
fork
curry
apply

; arithmetic primitives
add
mul

|#


(define expr?
  (flat-rec-contract expr?
                     (or/c (list/c 'quote any/c)
                           (list/c expr? expr?)
                           ; piping
                           'const
                           'id
                           'compose
                           ; pairs
                           'pair
                           'left
                           'right
                           'fork
                           ; functions
                           'curry
                           'apply
                           ; arith
                           'add
                           'mul
                           ;;
                           )))
(module+ test
  (check-pred expr? 'id)
  (check-pred expr? '(id '5))
  (check-false (expr? '(id 5)))
  (check-false (expr? #f))
  )


; false means no step: already a value.
; error in the object program means
(define/contract (step expr) (-> expr? (or/c #f expr?))
  (match expr
    [(? symbol?) #false]
    [`(quote ,v) #false]
    [`(,f ,a) (match (step f)
                [#false (match (step a)
                          [#false (app f a)]
                          [a* `(,f ,a*)])]
                [f* `(,f* ,a)])]))
(define/contract (app f a) (-> expr? expr? (or/c #f expr?))
  (match f
    ['const #f]
    [`(const ,v) v]
    ['id a]
    ['compose #f]
    [`(compose ,_) #f]
    [`((compose ,after) ,before) `(,after (,before ,a))]
    ['pair #f]
    [`(pair ,_) #f]
    ['left (match a [`((pair ,l) ,r) l])]
    ['right (match a [`((pair ,l) ,r) r])]
    ['fork #f]
    [`(fork ,_) #f]
    [`((fork ,left) ,right)
     `((pair (,left ,a)) (,right ,a))]
    ['curry #f]
    [`(curry ,f) `((compose ,f) ((fork (const ,a)) id))]
    ['apply (match a [`((pair ,f) ,a) `(,f ,a)])]
    ['add #f]
    [`(add ,x) (match* {x a} [{`(quote ,x) `(quote ,a)} `(quote ,(+ x a))])]
    ['mul #f]
    [`(mul ,x) (match* {x a} [{`(quote ,x) `(quote ,a)} `(quote ,(* x a))])]
    ;;
    ))
(define/contract (eval expr) (-> expr? expr?)
  (match (step expr)
    [#false expr]
    [expr* (eval expr*)]))
(module+ test
  (check-equal? (eval '(quote 5)) '(quote 5))
  (check-equal? (eval 'pair) 'pair)
  (check-equal? (eval '((mul '10) '2)) ''20)
  (check-equal? (eval '((add '10) '2)) ''12)

  ; (lambda (x) ((mul x) x))
  (define sqr '((compose apply) ((fork mul) id)))
  (check-equal? (eval `(,sqr (quote 4))) '(quote 16))

  (check-equal? (eval '((const '1) '2)) ''1)
  (check-equal? (eval '(left ((pair '1) '2))) ''1)
  (check-equal? (eval '(right ((pair '1) '2))) ''2)

  (check-equal? (eval '((((const mul) 'ignored) '10) '2)) ''20)
  (check-equal? (eval '(((curry left) '1) '2)) ''1)
  (check-equal? (eval '(((curry right) '1) '2)) ''2)

  ;;
  )


(define (steps! v)
  (match (step v)
    [#false (void)]
    [v (begin
         (displayln v)
         (steps! v))]))
(module+ test
  (check-equal? (with-output-to-string (lambda () (steps! '((add '1) '2))))
                "(quote 3)\n"))
