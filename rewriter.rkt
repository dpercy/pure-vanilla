#lang racket

(provide (all-from-out 'core)
         rewriter?
         iterate
         deep)

(require (only-in racket/syntax format-symbol))
(module+ test (require rackunit))

(module core racket
  (provide (all-defined-out))

  (define self-quoting?
    (not/c (or/c symbol?
                 empty?
                 cons?
                 void?)))

  (struct Expr () #:transparent
    #:methods gen:custom-write
    [(define (write-proc expr port mode)
       (write-string (format "(parse '~a)" (render expr))
                     port))])
  (struct Var Expr (name) #:transparent)
  (struct Quote Expr (value) #:transparent)
  (struct Lambda Expr (params body) #:transparent)
  (struct Call Expr (func args) #:transparent)
  (struct If Expr (test consq alt) #:transparent)

  (define (parse form)
    (match form
      [(? symbol?) (Var form)]
      [(? self-quoting? v) (Quote v)]
      [`(quote ,v) (Quote v)]
      [`(lambda (,params ...) ,body) (Lambda params (parse body))]
      [`(if ,t ,c ,a) (If (parse t) (parse c) (parse a))]
      [`(let ([,x ,v] ...) ,body) (parse `((lambda ,x ,body) ,@v))]
      [(cons (or 'quote 'lambda 'if 'let) _) (error 'parse "bad syntax: ~v" form)]
      [`(,f ,args ...) (Call (parse f) (map parse args))]
      [_ (error 'parse "bad syntax: ~v" form)]))

  (define (render form)
    (match form
      [(Quote v) `(quote ,v)]
      [(Var name) name]
      [(Lambda params body) `(lambda ,params ,(render body))]
      [(Call (Lambda params body) args) #:when (= (length params) (length args))
       `(let ,(map list params (map render args)) ,(render body))]
      [(Call func args) `(,(render func) ,@(map render args))]
      [(If t c a) `(if ,(render t) ,(render c) ,(render a))])))
(require 'core)
(module+ test
  (check-equal? (parse 0) (Quote 0))

  (define example '(let ([x (+ '1 '2)])
                     (if x
                         'foo
                         (lambda () x))))
  (check-equal? (render (parse example)) example)
  ;;
  )


; A rewriter is a function that makes a single transformation to a tree.
; It can also "fail to apply".
; A manually defined rewriter should usually be "shallow" and apply only once.
(define rewriter? (-> Expr? (or/c Expr? #false)))

(module+ test

  ; Example of a shallow rewriter.
  (define/contract dec-constant rewriter?
    (match-lambda
      [(Quote (? (and/c number? positive?) n)) (Quote (- n 1))]
      [_ #false]))

  ; It applies to a quoted positive number
  (check-equal? (dec-constant (Quote 3)) (Quote 2))
  ; It doesn't apply to anything else
  (check-equal? (dec-constant (Var 'x)) #false)
  (check-equal? (dec-constant (Quote 'hi)) #false)
  (check-equal? (dec-constant (Quote 0)) #false)
  (check-equal? (dec-constant (Quote -4)) #false)
  ; It doesn't search for subexpressions
  (check-equal? (dec-constant (Call (Quote 1) (list (Quote 2) (Quote 3)))) #false)

  ;;
  )

; Iterating a rewriter applies it *one or more* times.
; If the rewriter fails to apply one, then the iteration also fails.
(define/contract (iterate rewriter) (-> rewriter? rewriter?)
  (define (zero-or-more expr) (match (rewriter expr)
                                [#false expr]
                                [expr (zero-or-more expr)]))
  (define (once-or-more expr) (match (rewriter expr)
                                [#false #false]
                                [expr (zero-or-more expr)]))

  once-or-more)
(module+ test

  (define zero-constant (iterate dec-constant))

  ; applied zero times
  (check-equal? (zero-constant (Quote 0)) #false)
  ; applied once or more
  (check-equal? (zero-constant (Quote 5)) (Quote 0))
  (check-equal? (zero-constant (Quote 20)) (Quote 0))

  ; fails
  (check-equal? (zero-constant (Quote 'foo)) #false)
  (check-equal? (zero-constant (Call (Quote 1) (list (Quote 2) (Quote 3)))) #false)

  ;;
  )


(define/contract (on-list rewriter) (-> rewriter? (-> (listof Expr?)
                                                      (or/c (listof Expr?) #false)))
  (define (tr* exprs) ; exprs or false
    (match exprs
      ['() #false]
      [(cons hd tl)
       (match (rewriter hd)
         [#false (match (tr* tl)
                   [#false #false]
                   [tl (cons hd tl)])]
         [hd (cons hd tl)])]))
  tr*)
(module+ test
  ; nothing applies
  (check-equal? ((on-list dec-constant) (list)) #f)
  (check-equal? ((on-list dec-constant) (list (Var 'x))) #f)
  ; one thing applies
  (check-equal? ((on-list dec-constant)
                 (list (Var 'x) (Quote 3))) (list (Var 'x) (Quote 2)))
  ; first thing applies
  (check-equal? ((on-list dec-constant) (list (Var 'x) (Quote 3) (Quote 3)))
                (list (Var 'x) (Quote 2) (Quote 3))))


; Applies the rewriter to the first child that it applies to.
(define (on-children rewriter)
  (define (tr expr)
    (match expr
      [(Var _) #false]
      [(Quote _) #false]
      [(Lambda params body) (match (rewriter body)
                              [#false #false]
                              [body (Lambda params body)])]
      [(Call func args) (match ((on-list rewriter) (cons func args))
                          [#false #false]
                          [(cons func args) (Call func args)])]
      [(If t c a) (match ((on-list rewriter) (list t c a))
                    [#false #false]
                    [(list t c a) (If t c a)])]))
  tr)


; A deep rewriter applies the rewrite once, somewhere in the tree.
(define (deep rewriter)
  (define (tr expr)
    ; first try the root
    (match (rewriter expr)
      ; if that succeeds, we're done
      [(and (not #false) expr) expr]
      ; otherwise deep-rewrite the children
      [#false ((on-children tr) expr)]))
  tr)
(module+ test

  (check-equal? ((deep dec-constant) (If (Quote 1) (Quote 1) (Quote 1)))
                (If (Quote 0) (Quote 1) (Quote 1)))

  (check-equal? ((iterate (deep dec-constant)) (If (Quote 1) (Quote 1) (Quote 1)))
                (If (Quote 0) (Quote 0) (Quote 0)))

  (check-equal? ((deep dec-constant) (If (Quote 0) (Quote 0) (Quote 0)))
                #false)

  (define zero-all-constants (iterate (deep dec-constant)))
  (check-equal? (zero-all-constants (parse '(let ([x 5])
                                              (if x 7 (+ 2 3)))))
                (parse '(let ([x 0])
                          (if x 0 (+ 0 0))))))


(define (complete rewriter) (iterate (deep (iterate rewriter))))


(define Let?
  (match-lambda
    [(Call (Lambda params _) args) (= (length params) (length args))]
    [_ #false]))
(define (split-Let expr) ; (list ctx expr)
  (match expr
    [(Call (Lambda params body) args)  (list (lambda (body)
                                               (Call (Lambda params body) args))
                                             body)]))
(define/contract float-let-over-call rewriter?
  (match-lambda
    [(Call (? Let? func) args) (match (split-Let func)
                                 [(list ctx func)  (ctx (Call func args))])]
    [(Call func (list args-before ... (? Let? arg) args-after ...))
     (match (split-Let arg)
       [(list ctx arg)  (ctx (Call func (append args-before
                                                (list arg)
                                                args-after)))])]
    [_ #false]))
(module+ test
  (check-equal? (float-let-over-call (parse '((let ([a 1]) f) x)))
                (parse '(let ([a 1]) (f x))))

  (check-equal? (float-let-over-call (parse '(f (let ([b 1]) x))))
                (parse '(let ([b 1]) (f x))))

  (check-equal? (float-let-over-call (parse '((let ([a 1]) f) (let ([b 1]) x))))
                (parse '(let ([a 1]) (f (let ([b 1]) x)))))

  (check-equal? ((complete float-let-over-call) (parse '((let ([a 1]) f) (let ([b 1]) x))))
                (parse '(let ([a 1]) (let ([b 1]) (f x)))))

  )


(define/contract (fv expr) (-> Expr? (set/c symbol?))
  (match expr
    [(Quote _) (set)]
    [(Var name) (set name)]
    [(Call func args) (apply set-union (cons (fv func)
                                             (map fv args)))]
    [(If t c a) (set-union (fv t) (fv c) (fv a))]
    [(Lambda params body) (set-subtract (fv body) (list->set params))]))
(module+ test
  (check-equal? (fv (parse '(lambda (x) (if a (+ x y) c))))
                (set '+ 'y 'a 'c)))


(define (rename name used)
  (if (set-member? used name)
      (rename (inc-symbol name) used)
      name))
(define (inc-symbol name)
  (string->symbol (string-append (symbol->string name) "*")))
(define (rename* names used)
  (match names
    ['() '()]
    [(cons hd tl) (let ([hd* (rename hd used)])
                    (cons hd*
                          (rename* tl (set-add used hd*))))]))
(module+ test
  (check-equal? (rename* '(x y z) (set)) '(x y z))
  (check-equal? (rename* '(x y z) (set 'y)) '(x y* z))
  (check-equal? (rename* '(x y y*) (set 'y)) '(x y* y**))
  (check-equal? (rename* '(x y* y) (set 'y)) '(x y* y**))
  )


(define/contract (subst expr h) (-> Expr? (hash/c symbol? Expr?) Expr?)
  (let recur ([expr expr])
    (match expr
      [(Quote _) expr]
      [(Var name) (hash-ref h name (lambda () expr))]
      [(Call func args) (Call (recur func) (map recur args))]
      [(If t c a) (If (recur t) (recur c) (recur a))]
      [(Lambda params body)

       ; We need to avoid the params capturing things in the substitution.
       ; Easiest thing is to uniquely rename the parameter, but that requires gensym.

       ; What about using swaps/permutations?
       ; 1. param<->param* the body
       ; 2. subst the body
       ; 3. param*<->param back

       ; So if param appeared in the subst, effectively it gets renamed.
       ; But we still have to choose param* such that it doesn't collide.
       (define subst-fv (apply set-union (for/list ([e (in-hash-values h)])
                                           (fv e))))
       (define params* (rename* params subst-fv))
       (Lambda params*
               (subst body
                      ; Every parameter gets renamed, even if it just gets renamed to itself.
                      ; This ensures that subst-fv always accounts for all in-scope variables.
                      ; It also ensures that we stop replacing when a variable is shadowed.
                      (for/fold ([h h]) ([p params] [p* params*])
                        (hash-set h p (Var p*)))))])))
(module+ test

  (define p parse)

  (check-equal? (subst (p 'x) (hash 'x (p 'y))) (p 'y))
  (check-equal? (subst (p 'y) (hash 'x (p 'y))) (p 'y))
  (check-equal? (subst (p 'z) (hash 'x (p 'y))) (p 'z))

  ; shadowing introduces a new variable; stop replacing it
  (check-equal? (subst (p '(lambda (x) (+ x y)))
                       (hash 'x (p 1)
                             'y (p 2)))
                ; notice we didn't need to rename the parameter
                (p '(lambda (x) (+ x 2))))

  ; don't capture
  (check-equal? (subst (p '(lambda (x) (+ x y)))
                       (hash 'y (p 'x)))
                ; notice we can't rename the x in the substitution, because it's global.
                ; so we have to rename the parameter.
                (p '(lambda (x*) (+ x* x))))

  ;;
  )


; let-peel "peels off" the first binding of a let.
(define/contract let-peel rewriter?
  (match-lambda
    [(? Let? (Call (Lambda (cons param params) body) (cons arg args)))
     #:when (not (empty? params))

     ; Rename the parameter to avoid capturing anything in args.
     (define args-fv (apply set-union (set) (map fv args)))
     (define param* (rename param (set-union args-fv
                                             ; Also avoid colliding with any other param.
                                             (list->set params))))
     (Call (Lambda (list param*)
                   (Call (Lambda params
                                 (subst body (hash param (Var param*))))
                         args))
           (list arg))]
    [_ #false]))
(module+ test

  (check-equal? (let-peel (p '(let ([x y] [y x])
                                (+ x y))))
                ; The first x got renamed to avoid capturing the global x.
                (p '(let ([x* y])
                      (let ([y x])
                        (+ x* y))))))


; beta-fold inlines a single inlinable value.
; It can work together with let-peel to handle multi-arm let forms.
(define/contract beta-fold rewriter?
  (match-lambda
    [(Call (Lambda (list param) body) (list arg)) #:when (inlinable? arg)
     (subst body (hash param arg))]
    [_ #false]))

(define inlinable?
  (match-lambda
    [(Quote _) #true]
    [(Var _) #true]
    [(Lambda _ _) #true]
    [(If _ _ _) #false]
    [(Call _ _) #false]))

; Applies rw1, then rw2.
; Only fails if both fail.
(define/contract (chain2 rw1 rw2) (-> rewriter? rewriter? rewriter?)
  (define (rw expr)
    (match (rw1 expr)
      [#false (rw2 expr)]
      [expr (match (rw2 expr)
              [#false expr]
              [expr expr])]))
  rw)

(define complete-beta-fold (complete (chain2 let-peel beta-fold)))

(module+ test

  (check-equal? (complete-beta-fold (p '(let ([twice (lambda (f) (lambda (n) (f (f n))))]
                                              [add (lambda (x)
                                                     ; this local function must be removed
                                                     (lambda (y) (+ x y)))])
                                          ((twice (add 1)) 3))))
                ; All functions and constants are inlined.
                ; But (+ 1 3) is not defined as inlinable, so it stays let-bound.
                (p '(let ([y (+ 1 3)])
                      (+ 1 y)))))


; TODO it would be much more powerful + convenient if a rewriter could access the environment.
; For example:
;  - Is this '+ the global one? (so I can fold it)
;  - Is this 'f bound to a lambda expression?
;      - maybe I want to inline it
;      - maybe I want to complain about indirect calls
;  - Is this 'x bound to a constructor? (so I can project a field out)
;      - You need to make this decision at the use site, not the def site,
;        because you might only want to inline the value when it's being projected,
;        because maybe its address is observable.
