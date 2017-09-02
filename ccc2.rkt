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
     (Compose tuple-acceptor
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
    (check-equal? (app cosSinProd (Pair 2 3)) (Pair (cos 6) (sin 6))))

  )


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

          [(#%plain-app U V) #'(Compose (Apply)
                                        (Fork (reify (#%plain-lambda (x) U))
                                              (reify (#%plain-lambda (x) V))))]

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

  (reify (lambda (x) x))
  (reify (lambda (x) (quote 4)))
  (reify (lambda (x) (lambda (y) x)))
  (reify (lambda (x) (lambda (y) y)))

  (define mul (curry *))
  (define add (curry +))

  (define sqr (reify (lambda (x) ((mul x) x))))
  (app sqr 3)

  ;;
  )
