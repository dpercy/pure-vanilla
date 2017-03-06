#lang racket

(provide #%module-begin
         (rename-out [top-interaction #%top-interaction])
         Function
         value->syntax)

(require "runtime.rkt")
(require (prefix-in racket: racket))


(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . e)
     ; HACK: stuff like #%app, #%datum should just be exported in main.rkt,
     ; but I was getting "ambiguous identifier", so instead I locally bind them in
     ; each #%top-interaction expression.
     (with-syntax ([#%app   (datum->syntax stx '#%app)]
                   [#%datum (datum->syntax stx '#%datum)])
       #'(let-syntax
             ([#%app   (syntax-rules () [(_ . b) (racket:#%app . b)])]
              [#%datum (syntax-rules () [(_ . b) (racket:#%datum . b)])])
           e))]))

(module reader syntax/module-reader
  #:whole-body-readers? #true
  #:read (lambda (port) (map syntax->datum (parse-and-compile-module "#<input>" port)))
  #:read-syntax parse-and-compile-module
  #:language 'vanilla
  #:info (lambda (request default fallback)
           (case request
             #;
             [(color-lexer)
              ; see /Applications/Racket v6.8/share/pkgs/datalog/tool/syntax-color.rkt
              ;  fn get-syntax-token
              ]
             [else
              (print (list 'dont-understand request default fallback))
              (newline)
              (fallback request default)]))

  (require syntax/srcloc)
  (require racket/path)
  (require "./parse.rkt")
  (require "./compile.rkt")
  (require (only-in "./runtime.rkt" base-exported-names))

  (define (parse-and-compile-module source-name port)
    (define mod-name (string->symbol (path->string (file-name-from-path source-name))))
    (displayln (list 'parsing mod-name 'from source-name))
    (define ast (parse-port/imports port
                                    mod-name
                                    (hash 'Base (base-exported-names))
                                    source-name))
    ;;; (displayln (list 'got-ast ast))
    (define stx (compile ast mod-name))
    stx))
