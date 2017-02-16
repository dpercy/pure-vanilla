#lang s-exp syntax/module-reader
#:whole-body-readers? #true
#:read (compose syntax->datum parse-and-compile-module)
#:read-syntax parse-and-compile-module
#:language 'bindings

(require "./vanilla-parse.rkt")
(require "./vanilla-compile.rkt")


(define (parse-and-compile-module port)
  (compile (parse-port/imports port (hash))))

(module bindings racket
  (provide #%module-begin
           #%app
           quote
           lambda
           ))
