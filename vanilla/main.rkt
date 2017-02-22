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
  #:info (lambda (request default fallback)
           (case request

             [else
              (print (list 'dont-understand request default fallback))
              (newline)
              (fallback request default)]))

  (require syntax/srcloc)
  (require "./parse.rkt")
  (require "./compile.rkt")
  (require (only-in "./runtime.rkt" base-exported-names))

  (define (parse-and-compile-module source-name port)
    (define ast (parse-port/imports port
                                    (hash 'Base (base-exported-names))
                                    source-name))
    (define stx (compile ast))
    stx))
