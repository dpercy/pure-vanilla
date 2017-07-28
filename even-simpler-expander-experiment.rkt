#lang racket


#|


- goal: implement partial compiler using rewrite rules
- goal: automatically remove abstractions to help the rewrite rules


|#
(module+ test (require rackunit))

(module helpers racket
  (provide (all-defined-out))
  (define self-quoting?
    (not/c (or/c symbol?
                 empty?
                 cons?
                 void?))))
(require 'helpers)
(begin-for-syntax (require 'helpers))

(struct Just (value) #:transparent)


(struct Expr () #:transparent)
(struct Var Expr (name) #:transparent)
(struct Quote Expr (value) #:transparent)
(struct Lambda Expr (params body) #:transparent)
(struct Call Expr (func args) #:transparent)
(struct If Expr (test consq alt) #:transparent)

(define inlineable-arg? (or/c Var? Quote? Lambda?))

(define/contract (make-compiler prim-handler) (-> (-> any/c (or/c #f Just?))
                                                  (-> any/c any/c))
  (define (compile expr)
    (displayln (format "compile ~v" expr))
    (match (prim-handler expr)
      ; easy case: prim handler knows how to reduce this expression
      [(Just result) result]
      [#false
       ; otherwise, we need to reduce the current expression somehow
       (match expr
         ; Can we inline?
         [(Call (Lambda params body) args) #:when (and (= (length params)
                                                          (length args))
                                                       (andmap inlineable-arg? args))
          ; Since all the args are sipmle (side effect free),
          ; we can try just plugging them in.
          (define h (for/hash ([p params]
                               [a args])
                      (values p a)))
          (displayln (format "  subst ~v in ~v" h body))
          (compile (subst body h))]

         ; Can we commute let bindings?
         ; (let ([x (let ([y 5]) y)]) x)
         ; ==>
         ; (let ([y 5]) (let ([x y]) x))
         ; TODO eliminate this case by having Call simplify its args first?
         ; TODO this doesn't work with multiple args, or heterogenous args
         [(Call (Lambda (list x) final-body)
                (list (Call (Lambda (list y) arg-body)
                            (list init-v))))
          (displayln (format "  commute ~v = ~v above ~v = ~v" y init-v x arg-body))
          (compile (Call (Lambda (list y)
                                 (Call (Lambda (list x)
                                               final-body)
                                       (list arg-body)))
                         (list init-v)))]

         ; Can we simplify some child expressions?
         [(Call func args)


          ]


         [_ (error 'compile "no rule to compile this expression: ~v" expr)])]))
  compile)

(define (try-simplify-root-once expr)
  (match expr

    ))

(define (subst e h)
  ; TODO hygiene
  (define (r e) (subst e h))
  (match e
    [(Quote _) e]
    [(Var x) (hash-ref h x (lambda () e))]
    [(Lambda params body) (Lambda params (r body))]
    [(If t c a) (If (r t) (r c) (r a))]
    [(Call f args) (Call (r f) (map r args))]))

(define-syntax-rule (define-compiler name
                      [pat form ... exp] ...)
  (define name
    (make-compiler
     (match-lambda
       ; TODO convenient wrapper for patterns?
       [pat
        ; middle forms could be expressions, or #:when clauses
        form ...
        ; wrap each expression in a Just
        (Just exp)]
       ...
       ; if no case matched, return #false to indicate there is no rule for this expr
       [_ #false]))))
(module+ test

  (define (parse form)
    (match form
      [(? symbol?) (Var form)]
      [(? self-quoting? v) (Quote v)]
      [`(quote ,v) (Quote v)]
      [`(lambda (,params ...) ,body) (Lambda params (parse body))]
      [`(if ,t ,c ,a) (If (parse t) (parse c) (parse a))]
      [(cons (or 'quote 'lambda 'if) _) (error 'parse "bad syntax: ~v" form)]
      [`(,f ,args ...) (Call (parse f) (map parse args))]
      [_ (error 'parse "bad syntax: ~v" form)]))
  (define (p form)
    (displayln (format "\n\nparse ~v" form))
    (parse form))


  (struct Plus (left right) #:transparent)
  (struct Num (value) #:transparent)

  (define-compiler arith
    [(Call (Var '+) (list x y)) (Plus (arith x) (arith y))]
    [(Quote (? number? n)) (Num n)])

  ; simple cases
  (check-equal? (arith (p '(quote 4))) (Num 4))
  (check-equal? (arith (p '5)) (Num 5))
  (check-equal? (arith (p '(+ 5 (quote 2)))) (Plus (Num 5) (Num 2)))
  (check-equal? (arith (p '(+ (+ 1 2) (+ 3 4)))) (Plus (Plus (Num 1) (Num 2))
                                                       (Plus (Num 3) (Num 4))))
  ; failure cases, when the DSC can't compile something
  (check-exn exn:fail? (lambda () (arith (p '(quote "foo")))) "no rule")
  (check-exn exn:fail? (lambda () (arith (p '(+ 1 2 3)))) "no rule")
  (check-exn exn:fail? (lambda () (arith (p '(+)))) "no rule")
  (check-exn exn:fail? (lambda () (arith (p '(lambda () 1)))) "no rule")

  ; failure cases, when the program is invalid
  (check-exn exn:fail? (lambda () (arith (p '(lambda 1)))) "bad syntax")
  (check-exn exn:fail? (lambda () (arith (p '()))) "bad syntax")

  ; cases that rely on constant propagation
  (check-equal? (arith (p '((lambda (x) x) 5))) (Num 5))
  (check-equal? (arith (p '((lambda (x) x) ((lambda (y) y) 5)))) (Num 5))
  (check-equal? (arith (p '((lambda (x) ((lambda (y) y) x)) 5))) (Num 5))
  ; tricky - cases that rely on constant propagation of a non-quote-form
  ''(check-equal? (arith (p '((lambda (x) ((lambda (y) y) x)) (+ 2 3)))) (Plus (Num 2) (Num 3)))
  ''(check-equal? (arith (p '((lambda (x) ((lambda (y) y) (+ x 3))) 2))) (Plus (Num 2) (Num 3)))

  ; cases that rely on inlining
  (check-equal? (arith (p '((lambda (inc) (inc 7))
                            (lambda (x) (+ x 1)))))
                (Plus (Num 7) (Num 1)))

  ; cases that rely on propagating a global
  (check-equal? (arith (p '((lambda (plus) (plus 1 2))
                            +)))
                (Plus (Num 1) (Num 2)))

  ; inlining a higher-order function
  (check-equal? (arith (p '((lambda (twice)
                              (twice (lambda (n) (+ n 1))
                                     7))
                            (lambda (f arg)
                              (f (f arg))))))
                (Plus (Plus (Num 7) (Num 1)) (Num 1)))

  ((lambda (f arg)
     (f (f arg))) (lambda (n) (+ n 1))
   7)
  ((lambda (f arg)
     (f (f arg)))
   (lambda (n) (+ n 1))
   7)


  ; To test copy propagation (eliminating renames), extend the language with "holes":

  (struct Hole () #:transparent)

  (define-compiler (arith-on var-name)
    [(Call (Var '+) (list x y)) (Plus ((arith-on var-name) x)
                                      ((arith-on var-name) y))]
    [(Quote (? number? n)) (Num n)]
    [(Var (== var-name)) (Hole)])

  (check-equal? ((arith-on 'x) (p 'x)) (Hole))
  (check-exn exn:fail? (lambda () ((arith-on 'x) (p 'y))) "no rule")
  (check-equal? ((arith-on 'x) (p '(+ x 1))) (Plus (Hole) (Num 1)))
  (check-equal? ((arith-on 'x) (p '((lambda (y) (+ y 2)) x))) (Plus (Hole) (Num 2)))
  (check-equal? ((arith-on 'x) (p '((lambda (y) (+ y y)) x))) (Plus (Hole) (Hole)))
  (check-equal? ((arith-on 'x) (p '((lambda (y) (+ x y)) x))) (Plus (Hole) (Hole)))
  (check-exn exn:fail? (lambda () ((arith-on 'x)
                                   (p '((lambda (y) (+ x z))
                                        x))))
             "no rule")



  ; Previously, the expander was forced to inline things to allow
  ; the compile rules to apply.
  ; Once you add lambda to the compile rules, there's no need to inline.


  ; TODO test an example where only calls to known functions are allowed:
  ; (like C or GLSL)
  ; - call to a global is fine
  ; - call to a parameter is not allowed. higher order functions like map must be specialized or inlined.
  ; - call to a lambda is fine; it's compiled as a let, not a call.

  #|

  How would a DSC express that it handles only calls to known functions?
  Maybe a predicate on the identifier? like  (,(? known? f) ,args ...)
  This seems easy to add later.

  |#



  ;;
  )
