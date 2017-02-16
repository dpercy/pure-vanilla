#lang racket



(provide #%module-begin
         #%top-interaction
         )

(module reader syntax/module-reader
  #:whole-body-readers? #true
  #:read (lambda (port) (map syntax->datum (parse-and-compile-module "#<input>" port)))
  #:read-syntax parse-and-compile-module
  #:language 'vanilla

  (require "./parse.rkt")
  (require "./compile.rkt")
  (require (only-in racket provide))

  (define (parse-and-compile-module source-name port)
    (define ast (parse-port/imports port (hash)))
    (displayln ast)
    (define stx (compile ast))
    (displayln stx)
    (displayln (map syntax->datum stx))
    stx))
