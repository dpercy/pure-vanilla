#lang racket


#|


- goal: implement partial compiler using rewrite rules
- goal: automatically remove abstractions to help the rewrite rules


|#
(module+ test (require rackunit))




(define (expand form)
  ; - constants are done
  ; - if, lambda expand their children

  ; - variables bound to a quote *can* be replaced with that quote
  ;   - or can be done already
  ; - calls to known functions *can* inline
  ;   -
  )





(module+ test


  (struct Plus (left right) #:transparent)
  (struct Num (value) #:transparent)

  (define-compiler arith
    [(+ ,x ,y) (Plus (arith x) (arith y))]
    [(quote ,(? number? n)) (Num n)])

  ; simple cases
  (check-equal? (arith '(quote 4)) (Num 4))
  (check-equal? (arith '5) (Num 5))
  (check-equal? (arith '(+ 5 (quote 2))) (Plus (Num 5) (Num 2)))
  (check-equal? (arith '(+ (+ 1 2) (+ 3 4))) (Plus (Plus (Num 1) (Num 2))
                                                   (Plus (Num 3) (Num 4))))
  ; failure cases, when the DSC can't compile something
  (check-exn exn:fail? (lambda () (arith '(quote "foo"))) "no rule")
  (check-exn exn:fail? (lambda () (arith '(+ 1 2 3))) "no rule")
  (check-exn exn:fail? (lambda () (arith '(+))) "no rule")
  (check-exn exn:fail? (lambda () (arith '(lambda () 1))) "no rule")

  ; failure cases, when the program is invalid
  (check-exn exn:fail? (lambda () (arith '(lambda 1))) "bad syntax")
  (check-exn exn:fail? (lambda () (arith '())) "bad syntax")

  ; cases that rely on constant propagation
  (check-equal? (arith '((lambda (x) x) 5)) (Num 5))
  (check-equal? (arith '((lambda (x) x) ((lambda (y) y) 5))) (Num 5))
  (check-equal? (arith '((lambda (x) ((lambda (y) y) x)) 5)) (Num 5))
  ; tricky - cases that rely on constant propagation of a non-quote-form
  ; - unnecessary?
  (check-equal? (arith '((lambda (x) ((lambda (y) y) x)) (+ 2 3))) (Plus (Num 2) (Num 3)))
  (check-equal? (arith '((lambda (x) ((lambda (y) y) (+ x 3))) 2)) (Plus (Num 2) (Num 3)))

  ; cases that rely on inlining
  (check-equal? (arith '((lambda (inc) (inc 7))
                         (lambda (x) (+ x 1))))
                (Plus (Num 7) (Num 1)))

  ; cases that rely on propagating a global
  (check-equal? (arith '((lambda (plus) (plus 1 2))
                         +))
                (Plus (Num 7) (Num 1)))


  ; To test copy propagation (eliminating renames), extend the language with "holes":

  (struct Hole () #:transparent)

  (define-compiler (arith-on var-name)
    [(+ ,x ,y) (Plus ((arith-on var-name) x)
                     ((arith-on var-name) y))]
    [(quote ,(? number? n)) (Num n)]
    [,(== var-name) (Hole)])

  (check-equal? ((arith-on 'x) 'x) (Hole))
  (check-exn exn:fail? (lambda () ((arith-on 'x) 'y)) "no rule")
  (check-equal? ((arith-on 'x) '(+ x 1)) (Plus (Hole) 1))
  (check-equal? ((arith-on 'x) '((lambda (y) (+ y 2)) x)) (Plus (Hole) 2))
  (check-equal? ((arith-on 'x) '((lambda (y) (+ y y)) x)) (Plus (Hole) (Hole)))
  (check-equal? ((arith-on 'x) '((lambda (y) (+ x y)) x)) (Plus (Hole) (Hole)))
  (check-exn exn:fail? (lambda () ((arith-on 'x)
                                   '((lambda (y) (+ x z))
                                     x)))
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
