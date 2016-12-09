#lang typed/racket


(module+ test
  (require/typed rackunit
    [check-equal? (-> Any Any Void)]))


(struct Expr () #:transparent)

(struct Def ([name : Symbol]
             [value : Expr]) #:transparent)

(struct Var Expr ([name : Symbol]) #:transparent)

(struct Lit Expr ([value : (U Boolean
                              String
                              Symbol
                              Number
                              Null
                              )]) #:transparent)

; TODO maybe cons can be just a global function?
(struct Cons Expr ([head : Expr]
                   [tail : Expr]) #:transparent)

(struct Func Expr ([params : (Listof Symbol)]
                   [body : Expr]) #:transparent)

(struct App Expr ([func : Expr]
                  [args : (Listof Expr)]) #:transparent)

; TODO many many more primitives - subsume cons?
; - cons is different from first in that it's a constructor
; - cons is more like lit than like first/rest/plus, etc
; TODO conditionals

; For now errors have a statically fixed message - it's not an expression.
(struct Error Expr ([msg : String]) #:transparent)


; Values are just a subset of expressions.
; We're not going to define a type for them though,
; because Cons can be a value or not depending on its fields.

(: hash-remove* (All (K V) (-> (HashTable K V) (Listof K) (HashTable K V))))
(define (hash-remove* h keys)
  (match keys
    ['() h]
    [(cons k keys) (hash-remove* (hash-remove h k)
                                 keys)]))

(define (subst [expr : Expr] [vars : (HashTable Symbol Expr)]) : Expr
  (match expr
    [(Var name)  (hash-ref vars name (lambda () expr))]
    [(Lit value)  expr]
    [(Cons head tail)  (Cons (subst head vars)
                             (subst tail vars))]
    [(Func params body)  (Func params
                               (subst body (hash-remove* vars params)))]
    [(App func args)  (App (subst func vars)
                           (for/list ([a args])
                             (subst a vars)))]))
(module+ test
  ; easy cases
  (check-equal? (subst (Lit 0) (hash 'x (Lit 1)))
                (Lit 0))
  (check-equal? (subst (Var 'x) (hash 'x (Lit 1)))
                (Lit 1))
  (check-equal? (subst (Var 'y) (hash 'x (Lit 1)))
                (Var 'y))
  ; recur into cons
  (check-equal? (subst (Cons (Lit 0) (Cons (Var 'x) (Cons (Var 'y) (Lit '()))))
                       (hash 'x (Lit 1)))
                (Cons (Lit 0) (Cons (Lit 1) (Cons (Var 'y) (Lit '())))))
  ; recur into app
  (check-equal? (subst (App (Var 'f) (list (Var 'x) (Var 'y)))
                       (hash 'f (Lit 0)
                             'x (Lit 1)))
                (App (Lit 0)
                     (list (Lit 1) (Var 'y))))
  ; stop at shadowing
  (check-equal? (subst (App (Func '(x y)
                                  (Cons (Var 'x)
                                        (Cons (Var 'y)
                                              (Cons (Var 'z)
                                                    (Lit '())))))
                            (list (Var 'y) (Var 'z)))
                       (hash 'x (Lit 0)
                             'y (Lit 1)
                             'z (Lit 2)))
                (App (Func '(x y)
                           (Cons (Var 'x)
                                 (Cons (Var 'y)
                                       (Cons (Lit 2)
                                             (Lit '())))))
                     (list (Lit 1) (Lit 2)))))

(define-type Env (HashTable Symbol (Promise Expr)))
(define (env-ref [env : Env] [name : Symbol]) : Expr
  (define (exn-fail-promise? exn)
    (match exn
      [(exn:fail (regexp "^force: reentrant promise") _) #true]
      [_ #false]))
  (with-handlers ([exn-fail-promise? (lambda (exn)
                                       (Error "cyclic definition"))])
    (force (hash-ref env name
                     (lambda ()
                       (delay
                         (Error "unbound variable")))))))
(module+ test
  (check-equal? (env-ref (hash 'x (delay (Lit 123)))
                         'x)
                (Lit 123))
  (check-equal? (env-ref (hash 'x (letrec ([p : (Promise Expr) (delay (force p))])
                                    p))
                         'x)
                (Error "cyclic definition")))

(define empty-env : Env (hash))

(: eval-expr (->* (Expr) (Env) Expr))
(define (eval-expr expr [env empty-env])
  (match expr
    [(Var name) (env-ref env name)]
    [(Lit value) expr]
    [(Cons head tail) (match (eval-expr head env)
                        [(? Error? err) err]
                        [head*
                         (match (eval-expr tail env)
                           [(? Error? err) err]
                           [(not (Lit '()) (Cons _ _))
                            (Error "cons onto non-list")]
                           [tail*
                            (Cons head* tail*)])])]
    [(Func params body) expr]
    [(App func args) (match (eval-expr func env)
                       [(? Error? err) err]
                       [func*
                        (let ([args* (let recur : (U Error (Listof Expr))
                                          ([args args])
                                          (match args
                                            ['() '()]
                                            [(cons a args)
                                             (match (eval-expr a env)
                                               [(? Error? err) err]
                                               [a*
                                                (match (recur args)
                                                  [(? Error? err) err]
                                                  [(? list? args*)
                                                   (cons a* args*)])])]))])
                          (match args*
                            [(? Error? err) err]
                            [(? list? args*)
                             (match func*
                               [(Func params body)
                                ; toDO check arity
                                (if (= (length params)
                                       (length args*))
                                    (subst body (for/hash : (HashTable Symbol Expr)
                                                          ([p params]
                                                           [a args*])
                                                  (values p a)))
                                    (Error "arity mismatch"))]
                               [_ (Error "apply non-function")])]))])]
    [(? Error? err) err]))
(module+ test
  ; easy app case
  (check-equal? (eval-expr (App (Func '(x y z)
                                      (Cons (Var 'x)
                                            (Cons (Var 'y)
                                                  (Cons (Var 'z)
                                                        (Lit '())))))
                                (list (Lit 0)
                                      (Lit 1)
                                      (Lit 2))))
                (Cons (Lit 0)
                      (Cons (Lit 1)
                            (Cons (Lit 2)
                                  (Lit '())))))

  ; closure:
  ; (\x y -> [x, y]) 123  ==>  (\y -> [123, y])
  ; This case really encourages a substitution model.
  (check-equal? (eval-expr (App (Func '(x)
                                      (Func '(y)
                                            (Cons (Var 'x)
                                                  (Cons (Var 'y)
                                                        (Lit '())))))
                                (list (Lit 123))))
                (Func '(y)
                      (Cons (Lit 123)
                            (Cons (Var 'y)
                                  (Lit '())))))

  ; errors propagate up
  (check-equal? (eval-expr (Error "foo")) (Error "foo"))
  (check-equal? (eval-expr (Cons (Error "foo") (Lit '()))) (Error "foo"))
  ; first error wins
  (check-equal? (eval-expr (App (Lit 1) (list (Error "A") (Error "B"))))
                (Error "A"))
  (check-equal? (eval-expr (App (Error "F") (list (Error "A") (Error "B"))))
                (Error "F"))

  ; unbound variable is an error.
  (check-equal? (eval-expr (Var 'x)) (Error "unbound variable"))
  (check-equal? (eval-expr (Cons (Var 'x)
                                 (App (Func '(v) (Var 'v))
                                      (list (Lit '())))))
                (Error "unbound variable"))

  ; cons onto non-list is an error
  (check-equal? (eval-expr (Cons (Lit 0) (Lit 1))) (Error "cons onto non-list"))

  ; apply a non-function is an error
  (check-equal? (eval-expr (App (Lit 0) '())) (Error "apply non-function"))

  ; arity mismatch is an error
  (check-equal? (eval-expr (App (Func '() (Lit 0))
                                (list (Lit 1))))
                (Error "arity mismatch"))

  ; variable lookup is an error
  (check-equal? (eval-expr (Var 'x))
                (Error "unbound variable"))

  ;;
  )

(define (eval-defs [defs : (Listof Def)]) : (Listof Def)
  ; 1. create a cyclic env where every name is bound to a promise
  (define env : Env (for/hash : Env ([def defs])
                      (match def
                        [(Def name value)
                         (values name (delay (eval-expr value env)))])))
  ; 2. force all the promises
  (for/list ([def defs])
    (match def
      [(Def name _) (Def name (env-ref env name))])))
(module+ test

  ; variable lookup looks in defs
  (check-equal? (eval-defs (list (Def 'a (Var 'c))
                                 (Def 'b (Var 'a))
                                 (Def 'c (Lit 123))))
                (list (Def 'a (Lit 123))
                      (Def 'b (Lit 123))
                      (Def 'c (Lit 123))))

  ; variable lookup into error def is that error
  (check-equal? (eval-defs (list (Def 'a (Var 'c))
                                 (Def 'b (Var 'a))
                                 (Def 'c (Error "ow"))))
                (list (Def 'a (Error "ow"))
                      (Def 'b (Error "ow"))
                      (Def 'c (Error "ow"))))

  ; cyclic definition is an error
  (check-equal? (eval-defs (list (Def 'main (Var 'main))))
                (list (Def 'main (Error "cyclic definition"))))

  ; errors don't ruin all defs
  (check-equal? (eval-defs (list (Def 'a (Lit 123))
                                 (Def 'b (Error "derp"))
                                 (Def 'c (Var 'a))))
                (list (Def 'a (Lit 123))
                      (Def 'b (Error "derp"))
                      (Def 'c (Lit 123))))



  ;
  )
