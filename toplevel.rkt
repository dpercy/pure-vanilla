#lang racket

(provide (all-defined-out))

(require "ast.rkt"
         "parse.rkt"
         "eval.rkt")


(define (run-toplevel! in)
  (define modstore (make-hash))
  (for ([form (in-producer read eof-object? in)])
    (displayln (with-handlers ([exn:fail? values])
                 (eval (parse form) modstore)))))

;;(define (embed-mod mod-val)) ; -> DefMod
(module+ main

  (run-toplevel! (current-input-port))

  ;;
  )
