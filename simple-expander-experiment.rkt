#lang racket


(require (only-in racket/generator in-generator yield))
#|

Idea:
- simple expression expander
- custom set of primitive forms
- check each ID as a prim before desugaring

core:
x
(quote v)
(lambda (x ...) e)
(e e ...)
(if e e e)



|#



#|

TODO hygiene
Hygiene involves renaming identifiers to avoid collisions.
See that Dybvig paper.
Try to keep it simple by putting hygiene info only on identifiers.
Try to make it transparent by printing all the hygiene info concisely.
For super-simplicity, implement "mark" by just appending to the symbol!

|#



(define self-quoting?
  (not/c (or/c symbol?
               empty?
               cons?
               void?)))

(define current-step-table (make-parameter #f))
(define step-log (make-parameter #f))

; Step either expands the form forward by 1 step,
; or returns void to indicate the form is already fully expanded.
(define (step form)
  (match (current-step-table)
    [#false (step-impl form)]
    [tbl (hash-ref! tbl form (lambda ()
                               (define v (step-impl form))
                               (when (step-log)
                                 (displayln (format "~s -> ~s" form v)))
                               v))]))
(define (step-impl form)
  (match form
    ; primitive cases
    [(? symbol?)       (void)]
    [(? self-quoting?) (list 'quote form)]
    [(list 'quote _)   (void)]
    [(list 'lambda (? (listof symbol?) params) body) (match (step body)
                                                       [(? void?) (void)]
                                                       [body `(lambda ,params ,body)])]
    [(list 'if t c a)  (match (step-first-in-list (list t c a))
                         [(? void?) (void)]
                         [(list t c a) `(if ,t ,c ,a)])]
    [(cons (and head (or 'quote 'lambda 'if)) _)
     (error 'expand "~v: bad syntax: ~v" head form)]

    ; if head is a macro keyword, apply the macro
    [(cons 'and _) (macro-and form)]
    [(cons 'let _) (macro-let form)]

    ; otherwise it's a call.
    ; Calls just expand their children.
    [(cons _ _) (step-first-in-list form)]

    ['() (error 'expand "empty parens not allowed")]))
(define (step-first-in-list forms)
  ; If any form in the list can step, return a new list with that one stepped.
  ; Otherwise return void.
  (match forms
    ['() (void)]
    [(cons first rest) (match (step first)
                         [(? void?) (match (step-first-in-list rest)
                                      [(? void?) (void)]
                                      [rest (cons first rest)])]
                         [first (cons first rest)])]))

(define (expand form)
  (match (step form)
    [(? void?) form]
    [form (expand form)]))


(define (macro-and form)
  (match form
    [(list 'and) #true]
    [(list 'and x y) `(if ,x ,y #false)]
    [(list* 'and x rest) `(and x (and ,@rest))]))

(define (macro-let form)
  (match form
    [(list 'let (list (list lhs rhs) ...) body)
     `((lambda (,@lhs) ,body) ,@rhs)]))


(module+ test
  (require rackunit)


  (check-equal? (expand 'x) 'x)
  (check-equal? (expand '(f x)) '(f x))
  (check-equal? (expand '(if a b c)) '(if a b c))
  (check-equal? (expand '(and (and a b) c)) '(if (if a b '#f) c '#f))

  (check-equal? (expand '(f (and a b))) '(f (if a b '#f)))

  (check-equal? (expand '(lambda () (and 1 2))) '(lambda () (if '1 '2 '#f)))


  ;;
  )

(define (step-cached form)
  (match (current-step-table)
    [#false (void)]
    [tbl (hash-ref tbl form void)]))

(define (steps-cached form)
  (in-generator
   (let loop ([form form])
     (if (void? form)
         (void) ; yield no more values
         (begin
           (yield form)
           (loop (step-cached form)))))))
(module+ test
  (let ([form  '(f (and a b))])
    (check-equal? (sequence->list (steps-cached form)) (list form))
    (parameterize ([current-step-table (make-weak-hasheq)])
      (check-equal? (sequence->list (steps-cached form)) (list form))
      (expand form)
      (check-equal? (sequence->list (steps-cached form))
                    '[
                      (f (and a b))
                      (f (if a b #f))
                      (f (if a b '#f))
                      ]))))

(define (expand! v)
  (when (step-log)
    (displayln "")
    (displayln "")
    (displayln (format "expand! ~v" v)))
  (unless (current-step-table)
    (error 'expand! "no current-step-table"))
  (expand v)
  v)


(define-match-expander ever-stepped-to
  (syntax-rules ()
    [(_ pat) (app (lambda (form)
                    (let loop ([form form])
                      (match form
                        [(? void?) (void)]
                        [pat form]
                        [_ (loop (step-cached form))])))
                  pat)]))
(module+ test


  (current-step-table (make-weak-hasheq))

  (check-match (expand! 'x) (ever-stepped-to 'x))
  (check-match (expand! '1) (ever-stepped-to '(quote 1)))
  (check-match (expand! '(and (and a b) c)) (ever-stepped-to '(and (and a b) c)))
  (check-match (expand! '(and (and a b) c)) (ever-stepped-to '(if (and a b) c #f)))
  ; This one doesn't match, because (and (if )) never happened
  (check-match (expand! '(and (and a b) c)) (not (ever-stepped-to '(and (if a b #f) c))))
  ; But it does work if you nest the "ever steps to" pattern
  (current-step-table (expand! (make-weak-hasheq)))
  (check-match (expand! '(and (and a b) c)) (ever-stepped-to `(and ,(ever-stepped-to '(if a b #f)) c)))

  ; 1 never steps to #f
  (check-match (expand! 1) (not (ever-stepped-to #f)))
  ; everything eventually steps to void
  (check-match (expand! 1) (ever-stepped-to (? void?)))

  ; Subforms that expanded have steps-cached.
  ; But subforms that didn't expand (auxilliary forms like params in a lambda)
  ; don't have steps-cached.
  (match-let* ([form (expand! '(lambda () 1))]
               [`(lambda ,params ,body) form])
    (check-equal? (sequence->list (steps-cached form))
                  '[(lambda () 1) (lambda () (quote 1))])
    (check-equal? (sequence->list (steps-cached params))
                  '[()])
    (check-equal? (sequence->list (steps-cached body))
                  '[1 (quote 1)]))

  ; It's fine to wrap every single subform in &
  (check-match (expand! '(and (and x y) z))
               (ever-stepped-to (list (ever-stepped-to 'if)
                                      (ever-stepped-to (list (ever-stepped-to 'if)
                                                             (ever-stepped-to 'x)
                                                             (ever-stepped-to 'y)
                                                             (ever-stepped-to #f)))
                                      (ever-stepped-to 'z)
                                      (ever-stepped-to #f))))

  ;;
  )

(define-match-expander &
  (lambda (stx)
    (syntax-case stx (unquote)
      [(_ (unquote subpat)) #'subpat]
      [(_ (subpats ...))
       ; wrap each subpat only if it isn't a "..."
       (with-syntax ([(subpats ...) (for/list ([subpat (syntax->list #'(subpats ...))])
                                      (if (equal? '... (syntax-e subpat))
                                          subpat
                                          #`(& #,subpat)))])

         #'(ever-stepped-to (list subpats ...)))]
      [(_ subpat) #'(ever-stepped-to 'subpat)])))
(module+ test

  (check-match (expand! '(and (and 1 2) 3)) (& (and (and 1 2) 3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (if 1 2 #f) 3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (if (and 1 2) 3 #f)))

  (check-match (expand! '(and (and 1 2) 3)) (& (and (and '1  2)  3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (and  1 '2)  3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (and  1  2) '3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (and '1 '2)  3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (and '1  2) '3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (and  1 '2) '3)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (and '1 '2) '3)))

  (check-match (expand! '(and (and 1 2) 3)) (& (and ,x '3)) (equal? x '(and 1 2)))
  (check-match (expand! '(and (and 1 2) 3)) (& (if ,x ,y #f)) (and (equal? x '(and 1 2))
                                                                   (equal? y 3)))

  ; dotted subpattern
  (check-match (expand! '(and 1 2)) (& (and ,args ...)) (equal? args '(1 2)))
  (check-match (expand! '(and 1 2)) (& (if ,args ...)) (equal? args '(1 2 #f)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (and ,args ...) 3)) (equal? args '(1 2)))
  (check-match (expand! '(and (and 1 2) 3)) (& (and (if ,args ...) 3)) (equal? args '(1 2 #f)))

  ;;
  )

(module+ test


  (struct Expr () #:transparent)
  (struct Var Expr (name) #:transparent)
  (struct Quote Expr (value) #:transparent)
  (struct Lambda Expr (params body) #:transparent)
  (struct Call Expr (func args) #:transparent)
  (struct If Expr (test consq alt) #:transparent)
  (struct Let Expr (arms body) #:transparent)
  (struct Arm (name value) #:transparent)

  (define (compile-core expr)
    (parameterize ([current-step-table (make-weak-hasheq)])
      (expand! expr)
      (let r ([expr expr])
        (match expr
          ; specialized cases come first
          [(& ((lambda (,params ...) ,body) ,args ...)) #:when (= (length params) (length args))
           (Let (for/list ([p params]
                           [a args])
                  (Arm p (r a)))
                (r body))]

          ; general cases come last
          [(& ,(? symbol? x)) (Var x)]
          [(& (quote ,v)) (Quote v)]
          ; NOTE the bug: this pattern is not general enough:
          ; it should allow any number of params, not just one.
          [(& (lambda (,params) ,body))  (Lambda params (r body))]
          [(& (if ,t ,c ,a)) (If (r t) (r c) (r a))]
          ; NOTE this compound bug: because the lambda pattern fails to match
          ; valid examples like (lambda () 1), they fall through to the call case,
          ; which misinterprets them.
          [(& (,func ,args ...)) (Call (r func) (map r args))]))))

  (check-equal? (compile-core '(let ([x 5])
                                 (+ x 2)))
                (Let (list (Arm 'x (Quote 5)))
                     (Call (Var '+) (list (Var 'x) (Quote 2)))))

  (check-exn exn:fail? (lambda () (compile-core '(if))))

  ; tricky: see bugs in implementation of 'compile-core
  ;;(check-equal? (compile-core '(lambda () 1)) (Lambda '() (Quote 1)))
  ; again note the bug
  (check-equal? (compile-core '(lambda (x) x))
                (Lambda 'x (Var 'x)))

  ; Inlining does not happen for this compiler: it never needs to
  (check-equal? (compile-core '(let ([positive? (lambda (n) (< 0 n))])
                                 (lambda (doc)
                                   (positive? (hash-ref doc 'x)))))
                (Let (list (Arm 'positive?
                                (Lambda 'n (Call (Var '<) (list (Quote 0) (Var 'n))))))
                     (Lambda 'doc
                             (Call (Var 'positive?)
                                   (list (Call (Var 'hash-ref)
                                               (list (Var 'doc) (Quote 'x))))))))



  (define (query expr)
    (parameterize ([current-step-table ;;(make-weak-hasheq)
                    (make-hash)
                    ])
      (expand! expr)
      (match expr
        [(& (lambda (,doc-id) ,body))
         (let r ([expr body])
           ; parse a predicate on doc-id
           (match expr
             [(& (quote #true)) (hash)]
             [(& (< (hash-ref ,(== doc-id) (quote ,(? symbol? field)))
                    (quote ,value)))
              (hash field (hash '$lt value))]
             [(& (< (quote ,value)
                    (hash-ref ,(== doc-id) (quote ,(? symbol? field)))))
              (hash field (hash '$gt value))]
             [(& (equal? (hash-ref ,(== doc-id) (quote ,(? symbol? field)))
                         (quote ,value)))
              (hash field (hash '$eq value))]
             [(& (and ,terms ...))
              (foldr conjoin-query (hash) (map r terms))]))])))
  (define (conjoin-query a b)
    (for/hash ([key (set-union (hash-keys a) (hash-keys b))])
      (values key
              (conjoin-pred (hash-ref a key hash)
                            (hash-ref b key hash)))))
  (define (conjoin-pred a b)
    (for/hash ([kv (append (hash->list a)
                           (hash->list b))])
      (match kv [(cons k v) (values k v)])))

  (check-equal? (query '(lambda (doc) (< (hash-ref doc 'x) 4)))
                ; {x: {$lt: 4}}
                (hash 'x (hash '$lt 4)))
  (check-equal? (query '(lambda (doc) (equal? (hash-ref doc 'x) "foo")))
                ; {x: {$eq: "foo"}}
                (hash 'x (hash '$eq "foo")))
  (check-equal? (query '(lambda (doc) (< 10 (hash-ref doc 'x))))
                ; {x: {$gt: 10}}
                (hash 'x (hash '$gt 10)))

  (check-equal? (query '(lambda (doc) (and (< 10 (hash-ref doc 'x))
                                           (equal? (hash-ref doc 'y) "foo"))))
                ; {x: {$gt: 10}, y: {$eq: "foo"}}
                (hash 'x (hash '$gt 10)
                      'y (hash '$eq "foo")))
  (check-equal? (query '(lambda (doc) (and (< 10 (hash-ref doc 'y))
                                           (equal? (hash-ref doc 'y) "foo"))))
                ; {y: [{$gt: 10}, {$eq: "foo"}]}
                (hash 'y (hash '$gt 10
                               '$eq "foo")))

  (check-equal? (query '(lambda (doc) #true))
                ; {}
                (hash))

  ; TODO make this work
  ; This is a much more interesting example:
  ; part of the query logic is abstracted into a helper function.
  ; Can the query compiler remove this abstraction?
  '''(check-equal? (query '(let ([positive? (lambda (n) (< 0 n))])
                             (lambda (doc)
                               (positive? (hash-ref doc 'x)))))
                   (hash 'x (hash '$gt 0)))
  ; TODO once this works, what assumptions have I made?
  ; Did I assume inlining always terminates?


  ;;
  )
