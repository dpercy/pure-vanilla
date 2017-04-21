#lang racket

(provide (all-defined-out))

(require "ast.rkt"
         "parse.rkt"
         "eval.rkt"
         (only-in 2htdp/batch-io read-file))

(define (make-system-module modstore)
  (Mod 'System
       ; TODO make these effects
       (hash 'readFile read-file
             'parse (lambda (text) (parse (with-input-from-string text read)))
             'eval (lambda (ast) (eval ast modstore))
             'registerModule (lambda (mod)
                               (when (not (Mod? mod))
                                 (error "not an evaluated module: ~v" mod))
                               (define mn (Mod-name mod))
                               (when (hash-has-key? modstore mn)
                                 (error "module already exists: ~a" mn))
                               (hash-set! modstore mn mod))
             'moduleExists (lambda (mn)
                             (hash-has-key? modstore (string->symbol mn)))
             'getArgs (lambda ()
                        (vector->list (current-command-line-arguments))))))

(define (make-builtin-module)
  (Mod 'Builtin
       (hash 'isEmpty empty?
             'link cons
             'first first
             'rest rest
             'debug displayln
             'apply apply
             'makeVariadic (lambda (f) (lambda args (f args)))
             'error (lambda (msg) (error (~a msg)))
             '== equal?
             '< <
             'void void
             'strlen string-length
             'strcat string-append
             'slice substring
             'ord (lambda (s) (match (string->list s) [(list c) (char->integer c)]))
             'chr (lambda (i) (list->string (list (integer->char i)))))))

(define (Syntax.unpack tag ast)
  (define (tag? name) (equal? tag (Global 'Syntax name)))
  (define (unsymbol v) (if (symbol? v) (symbol->string v) v))
  (define (map-unsymbol v) (and v (map unsymbol v)))
  (map-unsymbol
   (match ast
     [(Module modname statements) (and (tag? 'Module) (list modname statements))]
     [(Using  modname)            (and (tag? 'Using)  (list modname))]
     [(Def    var expr)           (and (tag? 'Def)    (list var expr))]
     [(Lit    value)              (and (tag? 'Lit)    (list value))]
     [(Quote  subast)             (and (tag? 'Quote)  (list subast))]
     [(Local  name number)        (and (tag? 'Local)  (list name number))]
     [(Global mod name)           (and (tag? 'Global) (list mod name))]
     [(Func   params body)        (and (tag? 'Func)   (list params body))]
     [(Call   func args)          (and (tag? 'Call)   (list func args))]
     [(If     test consq alt)     (and (tag? 'If)     (list test consq alt))])))
(module+ test
  (require rackunit)
  (check-equal? (Syntax.unpack (Global 'Syntax 'Global) (Global 'm 'x))
                (list "m" "x")))

(define (make-syntax-module)
  (Mod 'Syntax
       (hash 'unpack Syntax.unpack)
       ))

(define (run-toplevel! in)
  (define modstore (make-hash (list (cons 'Builtin (make-builtin-module))
                                    (cons 'Syntax (make-syntax-module)))))
  (hash-set! modstore 'System (make-system-module modstore))
  (for ([form (in-producer read eof-object? in)])
    #;
    (with-handlers ([exn:fail? (lambda (exn)
                                 ((error-display-handler) (exn-message exn) exn))]))

    (match (parse form)
      [(? Module? ast) (let ([m (eval ast modstore)])
                         (hash-set! modstore (Mod-name m) m)
                         (displayln (format "module: ~v" m)))]
      [ast (let ([v (eval ast modstore)])
             (displayln (format "value: ~v" v)))])))

;;(define (embed-mod mod-val)) ; -> DefMod
(module+ main

  (match (vector->list (current-command-line-arguments))
    [(cons boot-script rest-args)
     (with-input-from-file boot-script
       (lambda ()
         (parameterize ([current-command-line-arguments
                         (list->vector rest-args)])
           (run-toplevel! (current-input-port)))))]
    [_ (error 'toplevel "specify a boot script")])

  ;;
  )
