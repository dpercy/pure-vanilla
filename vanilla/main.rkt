#lang racket

(require "runtime.rkt")

(provide #%module-begin
         #%top-interaction
         Function
         value->syntax
         )

(module reader syntax/module-reader
  #:whole-body-readers? #true
  #:read (lambda (port) (map syntax->datum (parse-and-compile-module "#<input>" port)))
  #:read-syntax parse-and-compile-module
  #:language 'vanilla

  (require "./parse.rkt")
  (require "./compile.rkt")

  (define (parse-and-compile-module source-name port)
    (define ast (parse-port/imports port (hash)))
    (displayln ast)
    (define stx (compile ast))
    (displayln stx)
    (displayln (map syntax->datum stx))
    stx))
