#lang racket

(provide (all-defined-out))

(require "ast.rkt"
         "parse.rkt"
         "eval.rkt"
         (only-in 2htdp/batch-io read-file))

(define (make-system-module modstore)
  (Mod 'System
       (hash 'readFile read-file
             'parse (lambda (text) (parse (with-input-from-string text read)))
             'eval (lambda (ast) (eval ast modstore))
             )))

(define (make-builtin-module)
  (Mod 'Builtin
       (hash 'isEmpty empty?
             'link cons
             'first first
             'rest rest
             'debug displayln
             )))

(define (run-toplevel! in)
  (define modstore (make-hash (list (cons 'Builtin (make-builtin-module)))))
  (hash-set! modstore 'System (make-system-module modstore))
  (for ([form (in-producer read eof-object? in)])
    (with-handlers ([exn:fail? (lambda (exn)
                                 ((error-display-handler) (exn-message exn) exn))])
      (define v (eval (parse form) modstore))
      (displayln (format "value: ~v" v))
      )))

;;(define (embed-mod mod-val)) ; -> DefMod
(module+ main

  (run-toplevel! (current-input-port))

  ;;
  )
