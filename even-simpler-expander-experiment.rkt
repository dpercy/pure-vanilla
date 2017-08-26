#lang racket


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

(define (subst e h)
  ; TODO hygiene
  (define (r e) (subst e h))
  (match e
    [(Quote _) e]
    [(Var x) (hash-ref h x (lambda () e))]
    [(Lambda params body)
     (define params* (map gensym params))
     (define h* (for/fold ([h h]) ([p params]
                                   [p* params*])
                  (hash-set h p (Var p*))))
     (Lambda params* (subst body h*))]
    [(If t c a) (If (r t) (r c) (r a))]
    [(Call f args) (Call (r f) (map r args))]))

; feature flags
(define known-call-supported? (make-parameter #false))

(define simpl-env (make-parameter (hash)))
(struct SimplBinding (residualized original simplified) #:transparent)
(define (lookup name)
  (hash-ref (simpl-env) name #false))
(define (prim? expr)
  (match expr
    [(Var (and (app lookup #false)
               (or '+)))
     #true]
    [_ #false]))
(define (trace name input output)
  (displayln (format "~s ~v ~s ~v" name input "->" output))
  output)
(define (simplify expr)
  (displayln (format "simpl ~v" expr))
  (trace
   "simplify"
   expr
   (match expr
     ; specialized cases
     ; - let1
     [(Call (Lambda (list param) body) (list arg))
      ; - is the bound name ever used?  else drop it!
      ; - is the bound name ever called?
      ; - is the bound name ever used as a non-function?
      ;     wait what if it's used as both?
      (let* ([orig-env (simpl-env)]
             [residualized? (box #false)]
             [arg* (delay (parameterize ([simpl-env orig-env])
                            (simplify arg)))]
             [binding (SimplBinding residualized?
                                    expr
                                    arg*)])
        (parameterize ([simpl-env (hash-set (simpl-env)
                                            param
                                            binding)])
          (let ([body* (simplify body)])
            (if (unbox residualized?)
                ; if the param was ever used, create a let binding
                (Call (Lambda (list param) body*) (list (force arg*)))
                ; if the param ended up being unused (maybe inlined),
                ; don't create any binding.
                ; But do still check for effects.
                (if (side-effect-free? (force arg*))
                    body*
                    (Call (Lambda (list param) body*) (list (force arg*))))))))]
     ; - var lookup
     [(Var (and x (app lookup (SimplBinding residualized? original simplified))))
      (let ([e (force simplified)])
        (if (side-effect-free? e)
            e
            (begin
              (set-box! residualized? #true)
              (Var x))))]

     ; lowering cases
     ; - known call
     [(Call (and (Var f) (app lookup (? SimplBinding? b))) args) #:when (not (known-call-supported?))
      ; We have to inline
      (match (force (SimplBinding-simplified b))
        ; success: callee simplified to a lambda expression
        [(and func (or (? prim?) (Lambda _ _))) (simplify (Call func args))]
        ; failure: callee simplified to something else
        [v
         (error 'simplify "Can't simplify this callee ~v bound to ~v any further in ~v"
                f
                v
                expr
                )])]

     ; general cases
     [(Var x)  (Var x)]
     [(Quote v)  (Quote v)]
     ; - NOTE: stop here to make ((lambda ) ) case easier
     ;         This must be why Dybvig uses a "call context" instead
     [(Lambda params body)  (Lambda params body)]
     [(Call func args)  (Call (simplify func) (map simplify args))]
     [(If t c a)  (If (simplify t) (simplify c) (simplify a))])))
(define (side-effect-free? expr)
  ;;  (displayln (list 'sef? expr))

  (match expr
    [(Quote _) #true]
    [(Var _) #true]
    [(Lambda _ _) #true]
    [(If t c a) (and (side-effect-free? t) (side-effect-free? c) (side-effect-free? a))]
    [(Call (Var (and '+ (app lookup #false)))
           args)
     (andmap side-effect-free? args)]
    [_
     (displayln (list 'nope expr))
     #false]))


(define-syntax-rule (define-compiler name arms ...)
  ; 1. inside the compiler, name is bound to just the final codegen step
  ; 2. outside the compiler, name is bound to simplify+codegen
  (define name
    (let ([the-compiler (letrec ([name (match-lambda arms ...
                                                     [e (error 'name "no rule for ~v" e)])])
                          name)])

      (lambda (expr)
        (the-compiler (simplify expr))))))
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
    (parse form))


  (struct Plus (left right) #:transparent)
  (struct Num (value) #:transparent)

  ; - primcalls
  ; - literals
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
  (check-equal? (arith (p '((lambda (x) ((lambda (y) y) x)) (+ 2 3)))) (Plus (Num 2) (Num 3)))
  (check-equal? (arith (p '((lambda (x) ((lambda (y) y) (+ x 3))) 2))) (Plus (Num 2) (Num 3)))

  ; cases that rely on inlining
  (displayln 'hi)
  (error 'TODO "reword tests in terms of simplify+flags")
  (check-equal? (arith (p '((lambda (inc) (inc 7))
                            (lambda (x) (+ x 1)))))
                (Plus (Num 7) (Num 1)))
  (displayln 'bye)


  ; cases that rely on propagating a global
  (check-equal? (arith (p '((lambda (add) (add 1 2))
                            +)))
                (Plus (Num 1) (Num 2)))

  ; inlining a higher-order function
  (displayln "")
  (displayln "")
  (displayln "")
  (check-equal? (arith (p '((lambda (twice)
                              (twice (lambda (n) (+ n 1))
                                     7))
                            (lambda (f arg)
                              (f (f arg))))))
                (Plus (Plus (Num 7) (Num 1)) (Num 1)))


  ; To test copy propagation (eliminating renames), extend the language with "holes":

  (struct Hole () #:transparent)

  ; - primcalls
  ; - literals
  ; - a single free/global variable
  (define-compiler arith-x
    [(Call (Var '+) (list x y)) (Plus (arith-x x)
                                      (arith-x y))]
    [(Quote (? number? n)) (Num n)]
    [(Var 'x) (Hole)])

  (check-equal? (arith-x (p 'x)) (Hole))
  (check-exn exn:fail? (lambda () (arith-x (p 'y))) "no rule")
  (check-equal? (arith-x (p '(+ x 1))) (Plus (Hole) (Num 1)))
  (check-equal? (arith-x (p '((lambda (y) (+ y 2)) x))) (Plus (Hole) (Num 2)))
  (check-equal? (arith-x (p '((lambda (y) (+ y y)) x))) (Plus (Hole) (Hole)))
  (check-equal? (arith-x (p '((lambda (y) (+ x y)) x))) (Plus (Hole) (Hole)))
  (check-exn exn:fail? (lambda () (arith-x
                                   (p '((lambda (y) (+ x z))
                                        x))))
             "no rule")

  (struct Bind1 (name val body) #:transparent)

  ; - primcalls
  ; - literals
  ; - let expressions
  ; - let-bound variables
  (define-compiler arith-let
    [(Call (Var '+) (list x y)) (Plus (arith-let x) (arith-let y))]
    [(Quote (? number? n)) (Num n)]
    [(Call (Lambda (list param) body) (list arg))
     ; WRONG --- you don't know whether the let is a number or function.
     ; If it's function you want to inline it, rather than compile a bind1.
     (Bind1 param
            (arith-let arg)
            (arith-let body))]
    [(Var x) x])

  (check-match (arith-let (p '((lambda (twice)
                                 (twice (lambda (n) (+ n 1))
                                        7))
                               (lambda (f arg)
                                 (f (f arg))))))
               (Bind1 x (Num 7)
                      (Bind1 y (Plus x (Num 1))
                             (Bind1 z (Plus y (Num 1))
                                    z))))


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
