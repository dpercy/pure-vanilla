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
    [(If t c a) `(if ,(render t) ,(render c) ,(render a))]))

; split an expression by peeling off any bindings around it.
(define (split expr) ; (list (-> expr expr) expr)
  (match expr
    [(Call (Lambda params body) args)

     (let ([ctx (lambda (body)
                  (Call (Lambda params body) args))])
       (match (split body)
         [(list ctx-inner body*) (list (compose ctx ctx-inner) body*)]))]
    [_ (list values expr)]))

#|

The goal of *simplify* is to remove certain language features.
- remove local function defs
- remove escaping functions/prims, and indirect calls
- remove all function defs, and non-prim calls
- remove let-binding

Mental model:
- let-binding is spelled ((lambda ) )
- variables can be:
.   - unbound ("global")
.   - let-bound
.   - parameters


Tactics:
- inline fun-calls to prevent their arguments from escaping
- inline fun-calls when the call isn't allowed
- inline let-bound values when let-binding isn't allowed
- drop unused let bindings when let-binding isn't allowed

- add parameters to def+use when local functions aren't allowed
.   - only works when the function doesn't escape
.   - can destroy known calls
- inline fun-defs when the local definition isn't allowed
.   - only works when the function doesn't escape
.   - could fail if inlining blows up

- lift bindings surrounding a callee to connect call and callee
- lift bindings surrounding an argument to make the argument inlinable
- generally, just always lift a let around a call.


Strategy:
- start with Dybvig's "unrestrained algorithm"
- add cycle detection - this is deterministic
- don't use effort counters - this is not really deterministic
- let the algorithm keep searching???


Which transformations enable each other?
- let-over-call causes more calls to become known
- known calls enable inlining
- known calls enable lifting
- lifting can reveal known calls
- inlining can reveal dead bindings

- global-lifting can *break* known bindings


|#

(struct BindLet (value env used simplified) #:transparent)
(struct BindParam () #:transparent)

(struct ContextValue () #:transparent)
(struct ContextCall (outer-context arg-binds) #:transparent)

(define (simplify expr
                  #:allow-let-binding [allow-let-binding #true]
                  #:allow-functions [allow-functions allow-let-binding]
                  #:allow-first-class-functions [allow-first-class-functions allow-functions])
  (when (and allow-first-class-functions (not allow-functions))
    (error 'simplify "allow-first-class-functions #true contradicts allow-functions #false"))
  (when (and allow-functions (not allow-let-binding))
    ; If you can't name your function,
    ; and you can't invoke it immediately like ((lambda ) ),
    ; then there's really no point in allowing functions at all.
    (error 'simplify "allow-functions #true contradicts allow-let-binding #false"))
  (displayln "")
  (displayln "")
  (displayln "")
  (let simplify ([expr expr]
                 [env (hash)]
                 [context (ContextValue)])
    (displayln (list 'simpl expr))
    (define (lookup name) (hash-ref env name #false))
    (match expr

      [(Quote _) expr]

      [(Var name) (let ()
                    (define (residualize)
                      (match (lookup name)
                        [(BindLet _ _ used _) (set-box! used #true)]
                        [_ (void)])
                      expr)
                    (match context

                      ; just residualize the var
                      [(ContextValue) (residualize)]

                      ; maybe can inline
                      [(ContextCall outer-context arg-binds)
                       (match (lookup name)

                         ; bound to a known operand
                         [(BindLet _ env* used simplified)
                          (match (force simplified)
                            [(list _ (? Lambda? lam))
                             ; operand is a function
                             ; need to inline if:
                             ; - functions aren't allowed
                             ; - an arg is a function, and first-class functions aren't allowed
                             ; - callee returns a function, and first-class functions aren't allowed
                             ; - otherwise don't inline

                             ; For now let's inline only if functions aren't allowed.
                             (cond
                               ; Instead of returning the var,
                               ; get the lambda it's bound to, and keep simplifying that.
                               [(not allow-functions)
                                (displayln (list 'inline name))
                                (define v (simplify lam env* context))
                                (displayln (list 'inlined name 'to v))
                                v]

                               ; decided not to inline - residualize
                               [else (residualize)])]

                            ; bound to something else
                            [_ (residualize)])]

                         ; not bound to a known operand
                         [_ (residualize)])]))]

      [(Lambda params body)

       (match context
         [(ContextValue)

          (Lambda params
                  (simplify body
                            (for/fold ([e env]) ([p params])
                              (hash-set e p (BindParam)))
                            (ContextValue)))]

         [(ContextCall outer-context arg-binds)

          ; This lambda is actually acting like a let.
          ; Extend the environment with its known arguments and recur on the body.
          ; But you have to preserve the params.
          (Lambda params
                  (simplify body
                            (for/fold ([e env]) ([p params] [a arg-binds])
                              (hash-set e p a))
                            ; The context for the body is the same as the context for the call.
                            ; This is how let-bindings are floated out over calls!
                            ; In (call (call (lambda (x) (lambda (y) E)) 1) 2),
                            ; the inner lambda will be process in (ContextCall ... (BindLet ... (Quote 2)).
                            context))])]

      [(Call func args)

       ; process the function in a call context
       (define arg-binds (for/list ([a args])
                           (BindLet a env (box #false)
                                    (delay
                                      (displayln (list 'operand a 'start))
                                      (let ([v (split (simplify a
                                                                env
                                                                (ContextValue)))])
                                        (displayln (list 'operand a 'got v))
                                        v)))))
       (define callee-context (ContextCall context arg-binds))
       (match-define (list func-wrapper func*) (split (simplify func env callee-context)))
       ; TODO check whether arguments were used.

       (match func*
         [(Lambda params body)
          ; If the function is a lambda, we can drop any unused bindings.

          ; TODO dedup this code with helpers / smart constructors like "make let"

          (define arg-wrapper (apply compose (for/list ([ab arg-binds]
                                                        #:when (and (BindLet? ab)
                                                                    (unbox (BindLet-used ab))))
                                               (match ab
                                                 [(BindLet _ _ _ (app force (list ctx _))) ctx]))))

          (func-wrapper
           (arg-wrapper
            (let ()
              (define params* (for/list ([p params]
                                         [ab arg-binds]
                                         #:when (and (BindLet? ab)
                                                     (unbox (BindLet-used ab))))
                                p))
              (define args* (for/list ([ab arg-binds]
                                       #:when (and (BindLet? ab)
                                                   (unbox (BindLet-used ab))))
                              (match ab
                                [(BindLet _ _ _ (app force (list _ simplified-value)))
                                 simplified-value])))
              (if (empty? params*)
                  body
                  (Call (Lambda params* body) args*)))))]
         [_

          ; Otherwise we have to keep all the unused arguments.
          (define arg-wrapper (apply compose (for/list ([ab arg-binds])
                                               (match ab
                                                 [(BindLet _ _ _ (app force (list ctx _))) ctx]))))

          (func-wrapper
           (arg-wrapper
            (Call func* (for/list ([ab arg-binds])
                          (match ab
                            [(BindLet _ _ _ (app force (list _ simplified-value)))
                             simplified-value])))))])]

      [(If test consq alt)
       ; TODO introduce ContextTest?
       (If (simplify test env (ContextValue))
           (simplify consq env context)
           (simplify alt env context))]

      [_ (list 'no-case-for expr)])))

(module+ test
  (require rackunit)


  (define (p form)
    (parse form))
  (check-equal? (p '(let ([x 1] [y 2]) 3))
                (Call (Lambda '(x y) (Quote 3))
                      (list (Quote 1) (Quote 2))))

  ; remove higher order functions (GLSL, C without function pointers)
  '''(check-equal? (simplify #:allow-first-class-functions #f
                             (p '(let ([twice (lambda (f) (lambda (x) (f (f x))))]
                                       [add (lambda (x)
                                              ; this local function must be removed
                                              (lambda (y) (+ x y)))])
                                   ((twice (add 1)) 3))))
                   ; - add got inlined because it returned a function
                   ; - twice got inlined because its argument was a function
                   ; - f got global-lifted because local functions aren't allowed
                   (p '(let ([x1 1]
                             [f (lambda (x y) (+ x y))])
                         (let ([x2 3]) (f x1 (f x1 x2))))))

  ; remove all functions (MongoDB aggregation expression)
  ; TODO bug here is just dead bindings
  (check-equal? (simplify #:allow-functions #f
                          (p '(let ([twice (lambda (f) (lambda (n) (f (f n))))]
                                    [add (lambda (x)
                                           ; this local function must be removed
                                           (lambda (y) (+ x y)))])
                                ((twice (add 1)) 3))))
                ; - add got inlined because it returned a function
                ; - twice got inlined because its argument was a function
                ; - f got inlined because functions aren't allowed
                (p '(let ([x 1])
                      (let ([n 3])
                        ; TODO actually this other let will lift,
                        ;      and both get renamed.
                        (let ([y (let ([y n])
                                   (+ x y))])
                          (+ x y))))))

  ; remove all functions, and let binding (MongoDB find expression)
  '''(check-equal? (simplify #:allow-let-binding #f
                             (p '(let ([twice (lambda (f) (lambda (x) (f (f x))))]
                                       [add (lambda (x)
                                              ; this local function must be removed
                                              (lambda (y) (+ x y)))])
                                   ((twice (add 1)) 3))))
                   ; - everything got inlined!!
                   ; - requires knowing that (+ 1 3) has no side effects and can be inlined!
                   (p '(+ 1 (+ 1 3))))

  ;;
  )
