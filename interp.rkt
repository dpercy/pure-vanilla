#lang racket

(require "ast.rkt")


(define (interp ast) ; -> Racket value
  (let interp ([ast ast]
               [globals (make-hash)]
               [locals (hash)])
    (match ast
      [(Global _ _) (hash-ref globals ast)]
      [(Local _ _) (hash-ref locals ast)]
      [(Lit v) v]
      [(Func params body) (procedure-reduce-arity (lambda args
                                                    (interp body
                                                            globals
                                                            (for/fold ([locals locals])
                                                                      ([p params]
                                                                       [a args])
                                                              (hash-set locals p a))))
                                                  (length params))]
      [(Call func args) (apply (interp func globals locals)
                               (for/list ([a args])
                                 (interp a globals locals)))])))
