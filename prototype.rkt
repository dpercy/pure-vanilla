#lang typed/racket


(module+ test
  (require/typed rackunit
    [check-equal? (-> Any Any Void)]))

; Values are a special case of expressions.
; Errors are also a special case of expressions.
; So evaluation can just consume and produce expressions.

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

(struct Cons Expr ([head : Expr]
                   [tail : Expr]) #:transparent)

(struct Tag Expr ([key : Expr]
                  [value : Expr]) #:transparent)

(struct Func Expr ([params : (Listof Symbol)]
                   [body : Expr]) #:transparent)

(struct App Expr ([func : Expr]
                  [args : (Listof Expr)]) #:transparent)

(struct If Expr ([test : Expr]
                 [consq : Expr]
                 [alt : Expr]) #:transparent)

(struct Prim Expr ([name : PrimName]) #:transparent)
(define-type PrimName
  (U 'empty?
     'cons?
     'first
     'rest
     'untag
     'tagged?
     ))

; TODO many many more primitives - subsume cons?
; - cons is different from first in that it's a constructor
; - cons is more like lit than like first/rest/plus, etc


; For now errors have a statically fixed message - it's not an expression.
(struct Error Expr ([msg : String]) #:transparent)


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
    [(Prim name) expr]
    [(Tag key value) (Tag (subst key vars)
                          (subst value vars))]
    [(Cons head tail)  (Cons (subst head vars)
                             (subst tail vars))]
    [(Func params body)  (Func params
                               (subst body (hash-remove* vars params)))]
    [(App func args)  (App (subst func vars)
                           (for/list ([a args])
                             (subst a vars)))]
    [(If test consq alt) (If (subst test vars)
                             (subst consq vars)
                             (subst alt vars))]))
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
(define (env-ref [env : Env] [name : Symbol]) : (U 'unbound 'cycle Expr)
  (define (exn-fail-promise? exn)
    (match exn
      [(exn:fail (regexp "^force: reentrant promise") _) #true]
      [_ #false]))
  (with-handlers ([exn-fail-promise? (lambda (exn)
                                       'cycle)])
    (force (hash-ref env name
                     (lambda ()
                       (delay 'unbound))))))
(module+ test
  (check-equal? (env-ref (hash 'x (delay (Lit 123)))
                         'x)
                (Lit 123))
  (check-equal? (env-ref (hash 'x (letrec ([p : (Promise Expr) (delay (force p))])
                                    p))
                         'x)
                'cycle))

(define empty-env : Env (hash))

(: eval-expr (->* (Expr) (Env) Expr))
(define (eval-expr expr [env empty-env])
  (match expr
    [(Var name) (match (env-ref env name)
                  ['unbound (Error "unbound variable")]
                  ['cycle (Error "cyclic definition")]
                  [(? Error?) (Error (format "relies on a bad definition: ~a" name))]
                  [(? Expr? v) v])]
    [(Lit value) expr]
    [(Prim name) expr]
    [(Tag key value) (match (eval-expr key env)
                       [(? Error? err) err]
                       [(and key* (Lit (? symbol?))) (match (eval-expr value env)
                                                       [(? Error? err) err]
                                                       [value* (Tag key* value*)])]
                       [_ (Error "tag key must be a symbol")])]
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
    [(If test consq alt) (match (eval-expr test env)
                           [(? Error? err) err]
                           [(Lit #true) (eval-expr consq env)]
                           [(Lit #false) (eval-expr alt env)]
                           [bad (Error (format "if non-boolean: ~v" bad))])]
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
                               [(Prim name)
                                (apply-prim name args*)]
                               [(Func params body)
                                (if (= (length params)
                                       (length args*))
                                    ; subst and then keep evaluating!
                                    (eval-expr (subst body (for/hash : (HashTable Symbol Expr)
                                                                     ([p params]
                                                                      [a args*])
                                                             (values p a)))
                                               env)
                                    (Error "arity mismatch"))]
                               [_ (Error "apply non-function")])]))])]
    [(? Error? err) err]))
(define (apply-prim [name : PrimName] [args : (Listof Expr)]) : Expr
  (define-syntax-rule (arity n expr)
    (if (not (= n (length args)))
        (Error "arity mismatch")
        expr))
  (match name
    ['empty? (arity 1 (match args
                        [(list (Lit '())) (Lit #true)]
                        [_ (Lit #false)]))]
    ['cons? (arity 1 (match args
                       [(list (Cons _ _)) (Lit #true)]
                       [_ (Lit #false)]))]
    ['first (arity 1 (match args
                       [(list (Cons head _)) head]
                       [(list bad) (Error (format "first non-cons: ~v" bad))]))]
    ['rest (arity 1 (match args
                      [(list (Cons _ tail)) tail]
                      [(list bad) (Error (format "rest non-cons: ~v" bad))]))]
    ['tagged? (arity 2 (match args
                         [(list (Lit (? symbol? s0))
                                (Tag (Lit (? symbol? s1)) _))  (Lit (symbol=? s0 s1))]
                         [_ (Lit #false)]))]
    ['untag (arity 2 (match args
                       [(list key0 tagged)
                        (match key0
                          [(Lit (? symbol? s0))
                           (match tagged
                             [(Tag (Lit (? symbol? s1)) value)
                              (if (symbol=? s0 s1)
                                  value
                                  (Error "untag key mismatch"))]
                             [_ (Error "untag arg must be a tagged value")])]
                          [_ (Error "untag key must be a symbol")])]))]))
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

  ; primitives
  (check-equal? (eval-expr (App (Prim 'first)
                                (list (Cons (Lit 1)
                                            (Cons (Lit 2)
                                                  (Lit '()))))))
                (Lit 1))
  (check-equal? (eval-expr (App (Prim 'rest)
                                (list (Cons (Lit 1)
                                            (Cons (Lit 2)
                                                  (Lit '()))))))
                (Cons (Lit 2)
                      (Lit '())))
  (check-equal? (eval-expr (App (Prim 'empty?)
                                (list (Cons (Lit 1)
                                            (Cons (Lit 2)
                                                  (Lit '()))))))
                (Lit #false))
  (check-equal? (eval-expr (App (Prim 'cons?)
                                (list (Cons (Lit 1)
                                            (Cons (Lit 2)
                                                  (Lit '()))))))
                (Lit #true))

  ; conditionals
  (check-equal? (eval-expr (If (Lit #true)
                               (Lit 'yes)
                               (Error "no")))
                (Lit 'yes))
  (check-equal? (eval-expr (If (Lit #false)
                               (Error "no")
                               (Lit 'yes)))
                (Lit 'yes))
  ; if on non-boolean is an error
  (check-equal? (eval-expr (If (Lit 5)
                               (Lit "hello")
                               (Lit "goodbye")))
                (Error "if non-boolean: (Lit 5)"))
  ; if error is the first error
  (check-equal? (eval-expr (If (Error "moe")
                               (Error "larry")
                               (Error "curly")))
                (Error "moe"))

  ; data abstraction - tagging and untagging.
  ; A symbol is a capability for identifying, creating, and inspecting tagged values.
  ; - tagging
  (check-equal? (eval-expr (Tag (If (Lit #true) (Lit 'dog) (Error "no"))
                                (If (Lit #false) (Error "no") (Lit "fido"))))
                (Tag (Lit 'dog)
                     (Lit "fido")))
  ; - tag must be a symbol
  (check-equal? (eval-expr (Tag (Lit "dog")
                                (Lit "fido")))
                (Error "tag key must be a symbol"))
  ; - check tag
  (check-equal? (eval-expr (App (Prim 'tagged?)
                                (list (Lit 'dog)
                                      (Tag (Lit 'dog) (Lit "fido")))))
                (Lit #true))
  (check-equal? (eval-expr (App (Prim 'tagged?)
                                (list (Lit 'cat)
                                      (Tag (Lit 'dog) (Lit "fido")))))
                (Lit #false))
  ; - untagging
  (check-equal? (eval-expr (App (Prim 'untag)
                                (list (Lit 'dog)
                                      (Tag (Lit 'dog)
                                           (Lit "fido")))))
                (Lit "fido"))
  ; - untag key must be a symbol
  (check-equal? (eval-expr (App (Prim 'untag)
                                (list (Lit "dog")
                                      (Tag (Lit 'dog)
                                           (Lit "fido")))))
                (Error "untag key must be a symbol"))
  ; - untag 2nd arg must be a tagged value
  (check-equal? (eval-expr (App (Prim 'untag)
                                (list (Lit 'dog)
                                      (Lit 57))))
                (Error "untag arg must be a tagged value"))
  ; - untag keys must match
  (check-equal? (eval-expr (App (Prim 'untag)
                                (list (Lit 'cat)
                                      (Tag (Lit 'dog)
                                           (Lit "fido")))))
                (Error "untag key mismatch"))

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
      [(Def name _) (Def name (match (env-ref env name)
                                ['cycle (Error "cyclic definition")]
                                ['unbound (error "unreachable")]
                                [(? Expr? e) e]))])))
(module+ test

  ; variable lookup looks in defs
  (check-equal? (eval-defs (list (Def 'a (Var 'c))
                                 (Def 'b (Var 'a))
                                 (Def 'c (Lit 123))))
                (list (Def 'a (Lit 123))
                      (Def 'b (Lit 123))
                      (Def 'c (Lit 123))))

  ; variable lookup into error def is a different error
  (check-equal? (eval-defs (list (Def 'a (Var 'c))
                                 (Def 'b (Var 'a))
                                 (Def 'c (Error "ow"))))
                (list (Def 'a (Error "relies on a bad definition: c"))
                      (Def 'b (Error "relies on a bad definition: a"))
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

  ; example program
  (check-equal? (fourth (eval-defs
                         (list (Def 'map (Func '(f lst)
                                               (If (App (Prim 'cons?)
                                                        (list (Var 'lst)))
                                                   (Cons (App (Var 'f)
                                                              (list (App (Prim 'first)
                                                                         (list (Var 'lst)))))
                                                         (App (Var 'map)
                                                              (list (Var 'f)
                                                                    (App (Prim 'rest)
                                                                         (list (Var 'lst))))))
                                                   (Lit '()))))
                               (Def 'example (Cons (Lit 1)
                                                   (Cons (Lit 'x)
                                                         (Cons (Lit "foo")
                                                               (Lit '())))))
                               (Def 'func (Func '(x)
                                                (Cons (Var 'x) (Cons (Var 'x) (Lit '())))))
                               (Def 'result (App (Var 'map)
                                                 (list (Var 'func)
                                                       (Var 'example)))))))
                (Def 'result (Cons (Cons (Lit 1) (Cons (Lit 1) (Lit '())))
                                   (Cons (Cons (Lit 'x) (Cons (Lit 'x) (Lit '())))
                                         (Cons (Cons (Lit "foo") (Cons (Lit "foo") (Lit '())))
                                               (Lit '()))))))



  ;
  )
