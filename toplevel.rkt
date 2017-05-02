#lang racket

(provide (all-defined-out))

(require "ast.rkt"
         "parse.rkt"
         "eval.rkt"
         "constructor.rkt"
         (only-in 2htdp/batch-io read-file))

(define (make-system-module modstore)
  (Mod "System"
       ; TODO make these effects
       (hash "readFile" read-file
             "parse" (lambda (text) (parse (with-input-from-string text read)))
             "eval" (lambda (ast) (eval ast modstore))
             "registerModule" (lambda (mod)
                                (when (not (Mod? mod))
                                  (error "not an evaluated module: ~v" mod))
                                (define mn (Mod-name mod))
                                (when (hash-has-key? modstore mn)
                                  (error "module already exists: ~a" mn))
                                (hash-set! modstore mn mod))
             "moduleExists" (lambda (mn)
                              (hash-has-key? modstore mn))
             "getArgs" (lambda ()
                         (vector->list (current-command-line-arguments))))))

(define (make-builtin-module)
  (Mod "Builtin"
       (hash "isEmpty" empty?
             "link" cons
             "first" first
             "rest" rest
             "debug" (lambda (x) (begin (print x) (newline) x))
             "apply" apply
             "makeVariadic" (lambda (f) (lambda args (f args)))
             "error" (lambda (msg) (error (~v msg)))
             "tag" tag
             "untag" untag
             "==" equal?
             "<" <
             "+" +
             "-" -
             "*" *
             "void" void
             "strlen" string-length
             "strcat" string-append
             "slice" substring
             "ord" (lambda (s) (match (string->list s) [(list c) (char->integer c)]))
             "chr" (lambda (i) (list->string (list (integer->char i)))))))

(define (run-toplevel! in)
  (define modstore (make-hash (list (cons "Builtin" (make-builtin-module)))))
  (hash-set! modstore "System" (make-system-module modstore))
  (for ([form (in-producer read eof-object? in)])
    #;
    (with-handlers ([exn:fail? (lambda (exn)
                                 ((error-display-handler) (exn-message exn) exn))]))

    (match (parse form)
      [(? Module? ast) (let ([m (eval ast modstore)])
                         (hash-set! modstore (Mod-name m) m)
                         ;(displayln (format "module: ~v" m))
                         (void))]
      [ast (let ([v (eval ast modstore)])
             ;(displayln (format "value: ~v" v))
             (void))])))

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
