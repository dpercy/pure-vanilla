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

; Step either expands the form forward by 1 step,
; or returns void to indicate the form is already fully expanded.
(define (step form)
  (match (current-step-table)
    [#false (step-impl form)]
    [tbl (hash-ref! tbl form (lambda ()
                               (define v (step-impl form))
                               (displayln (format "~s -> ~s" form v))
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
  (displayln "")
  (displayln "")
  (displayln (format "expand! ~v" v))
  (unless (current-step-table)
    (error 'expand! "no current-step-table"))
  (expand v)
  v)


(define-match-expander &
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

  (check-match (expand! 'x) (& 'x))
  (check-match (expand! '1) (& '(quote 1)))
  (check-match (expand! '(and (and a b) c)) (& '(and (and a b) c)))
  (check-match (expand! '(and (and a b) c)) (& '(if (and a b) c #f)))
  ; This one doesn't match, because (and (if )) never happened
  (check-match (expand! '(and (and a b) c)) (not (& '(and (if a b #f) c))))
  ; But it does work if you nest the "ever steps to" pattern
  (displayln "")
  (current-step-table (expand! (make-weak-hasheq)))
  (check-match (expand! '(and (and a b) c)) (& `(and ,(& '(if a b #f)) c)))
  (displayln "")

  ; 1 never steps to #f
  (check-match (expand! 1) (not (& #f)))
  ; everything eventually steps to void
  (check-match (expand! 1) (& (? void?)))

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

  ;;
  )
