#lang racket


(require (prefix-in racket: racket))
(module+ test (require rackunit))

(define-syntax-rule (define-structs (Name fields ...) ...)
  (begin
    (struct Name (fields ...) #:transparent) ...))

(define-structs
  (Pair left right)
  ;;
  )

(define-structs
  (Const value)
  (Id)
  (Compose after before)
  (Fork left right)
  (Curry tuple-acceptor)
  (Apply)
  ;;
  )


(define (app fun arg)
  (match fun
    [(? procedure?) (fun arg)]
    [(Const value) value]
    [(Id) arg]
    [(Compose after before) (app after (app before arg))]
    [(Fork left right) (Pair (app left arg) (app right arg))]
    [(Curry tuple-acceptor)
     ; A curried tuple-acceptor accepts the left,
     ; and returns a function that accepts the right and calls the original.

     ; The new argument flows into Id, gets paired with arg,
     ; then flows into tuple-acceptor.
     (smart-Compose tuple-acceptor
                    (Fork (Const arg)
                          (Id)))]
    [(Apply)
     ; Apply accepts a (Pair f a) and returns (f a).
     (match arg
       [(Pair f a) (app f a)]
       [_ (error 'Apply "got a non-pair: ~v" arg)])]))
(module+ test

  (let ()
    (define addC (match-lambda [(Pair x y) (+ x y)]))
    (define mulC (match-lambda [(Pair x y) (* x y)]))
    (define cosC cos)
    (define sinC sin)
    (define exl Pair-left)
    (define exr Pair-right)

    (define sqr (Compose mulC (Fork (Id) (Id))))
    (check-equal? (app sqr 4) 16)

    (define magSqr (Compose addC
                            (Fork (Compose mulC (Fork exl exl))
                                  (Compose mulC (Fork exr exr)))))
    (check-equal? (app magSqr (Pair 3 4)) 25)

    (define cosSinProd (Compose (Fork cosC sinC) mulC))
    (check-equal? (app cosSinProd (Pair 2 3)) (Pair (cos 6) (sin 6)))



    #|

    What is this stuff??

    1. Pipes

    (Id)  is a an empty piece of pipe
    (Compose after before)   lets you fit pipes together

    2. Tuples
    (Fork left right)  lets you put pipes next to each other, creating double-wires
    exl                extracts a single value from a double-wire
    exr


    (Curry tuple-acceptor)
    ; converts an (X, Y) acceptor into an X-acceptor-giving-Y-acceptor






    |#


    ;;
    ))

(define (smart-Compose after before)
  (match* {after before}
    [{(Id) f} f]
    [{f (Id)} f]
    [{(Const v) _} (Const v)]

    ; This case is iffy if the function has side effects
    [{f (Const v)} (Const (app f v))]

    [{(== Pair-left) (Fork left right)} left]
    [{(== Pair-right) (Fork left right)} right]
    #;
    [{(Apply) (Fork (Compose (Curry f) (== Pair-left))
                    (== Pair-right))}
     ; "universal property" from p2 http://conal.net/papers/compiling-to-categories/compiling-to-categories.pdf
     f]

    [{(Apply) (Fork (Const f) arg)}
     ; If the function doesn't depend on the input to the Compose,
     ; it doesn't need to be a first-class function.
     (smart-Compose f arg)]
    [{_ _} (Compose after before)]))

(require (for-syntax racket))
(begin-for-syntax
  (define-syntax-rule (replacer expr)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(_ forms (... ...)) #'(expr forms (... ...))]
         [(set! _ v) #'(set! expr v)]
         [_ #'expr])))))
(define-syntax (reify stx)
  (define (peel form) (local-expand form 'expression
                                    '()))

  (syntax-case stx ()
    [(_ form)

     (syntax-case (peel #'form) (#%plain-lambda quote #%plain-app)
       [(#%plain-lambda (x) B)

        (syntax-case (peel #'B) (#%plain-lambda quote #%plain-app)
          [y (identifier? #'y) (if (bound-identifier=? #'x #'y)
                                   #'(Id)
                                   #'(Const y))]
          [(quote v) #'(Const (quote v))]


          [(#%plain-app U V) #'(smart-Compose (Apply)
                                              (Fork (reify (#%plain-lambda (x) U))
                                                    (reify (#%plain-lambda (x) V))))]

          ; Pair case!
          [(#%plain-app Pair left right) #'(Fork (reify (#%plain-lambda (x) left))
                                                 (reify (#%plain-lambda (x) right)))]

          [(#%plain-lambda (y) U)
           #'(Curry (reify
                     (#%plain-lambda ; function that accepts (x, y) pairs p
                      (p)
                      (let-syntax ([x (replacer (Pair-left p))]
                                   [y (replacer (Pair-right p))])
                        U))))]

          [(#%expression body) #'(reify (#%plain-lambda (x) body))]
          [(let-values () body) #'(reify (#%plain-lambda (x) body))]


          [e (raise-syntax-error 'reify "no inner case" stx #'e)])]


       ; final fallthrough case
       [e (raise-syntax-error 'reify "no outer case" stx #'e)])]))


(module+ test

  (check-equal? (reify (lambda (x) x)) (Id))
  (check-equal? (reify (lambda (x) (quote 4))) (Const 4))
  (check-equal? (reify (lambda (x) (lambda (y) x))) (Curry Pair-left))
  (check-equal? (reify (lambda (x) (lambda (y) y))) (Curry Pair-right))

  (define mul (curry *))
  (define add (curry +))

  (define mulPair (match-lambda [(Pair x y) (* x y)]))
  (define addPair (match-lambda [(Pair x y) (+ x y)]))

  (define sqr (reify (lambda (x) ((mul x) x))))
  (app sqr 3)

  (check-equal? (app (app (reify (lambda (x) (lambda (y) x))) 1) 2)
                1)
  (check-equal? (app (app (reify (lambda (x) (lambda (y) y))) 1) 2)
                2)

  (check-equal? (app (reify (lambda (x) 4)) (void))
                4)

  (define magsqr (reify (lambda (p)
                          (addPair (Pair (mulPair (Pair (Pair-left p)
                                                        (Pair-left p)))
                                         (mulPair (Pair (Pair-right p)
                                                        (Pair-right p))))))))
  (check-equal? (app magsqr (Pair 3 4))
                25)

  ;;
  )
