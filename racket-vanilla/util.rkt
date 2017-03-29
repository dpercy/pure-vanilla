#lang racket

(provide (all-defined-out))

(define (memoize table func)
  (define (new-func arg)
    (hash-ref! table arg
               (lambda () (func arg))))
  (if (object-name func)
      (procedure-rename new-func (object-name func))
      new-func))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))
