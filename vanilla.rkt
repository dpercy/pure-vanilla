#lang racket

(define-syntax-rule (proc new-name racket-value)
  (begin
    (define new-name (procedure-rename racket-value 'new-name))
    (provide new-name)))

(provide (rename-out [define/thunk def]
                     [transparent-lambda lambda]
                     [module-begin #%module-begin]
                     [cond-no-void cond]
                     ;;[cond cond]
                     [else else])
         #%app
         #%datum
         #%top-interaction

         if
         let ; TODO ban letrec / let loop?
         let*

         <
         +
         -
         list
         first
         rest
         cons
         empty?

         equal?
         error
         )
(proc show (lambda (v) (format "~v" v)))
(proc concat (lambda (lists) (apply append lists)))
(proc split (lambda (str sep) (string-split str sep)))
(proc slice substring)
(proc replicate make-list)
(proc parse-int string->number)
(proc length (match-lambda
               [(? string? s) (string-length s)]
               [lst (length lst)]))

(define-syntax (not-allowed-to-set stx)
  (syntax-case stx ()
    [(_ var) (raise-syntax-error #f
                                 "not allowed to set! a pure-vanilla variable"
                                 #'var)]))

(define (reentrant-promise? exn)
  (and (exn:fail? exn)
       (string-prefix? (exn-message exn)
                       "force: reentrant promise")))


#|

define/thunk defines a variable as a thunk which is transparently
forced when the variable is used. for example:

(define/thunk x (begin (displayln "foo") 123))
(displayln "bar")
(displayln (let ([v x])
             (displayln "quux")
             v))

displays:

; initially nothing, because x is a thunk on line 1
"bar"   ; on line 2
"foo"   ; expression #'x on line 3
"quux"  ; on line 4
123     ; #'v returns to the (displayln (let ...))

TODO fix this in the repl:
(define/thunk y x)
(define/thunk x 1)
y  ; x: undefined
Because the identifier x gets defined as a macro, after y is defined.
Solution must involve defining #%top, and being able to consistently
derive the x-thunk identifier from x.
The (#%top . x) can desugar to (force x-thunk).

|#
(define-syntax (define/thunk stx)
  (syntax-case stx ()
    [(_ x e)
     (with-syntax ([(x-thunk) (generate-temporaries (list #'x))])
       #'(begin
           (define x-thunk (delay
                             (with-handlers ([reentrant-promise?
                                              (lambda (exn)
                                                (error "pure-vanilla: cyclic definition"))])
                               ; use let as a name hint for e
                               (let ([x e])
                                 x))))
           (define-syntax x
             (syntax-id-rules (set!)
               [(set! _ _) (not-allowed-to-set x)]
               [(x args (... ...)) ((force x-thunk) args (... ...))]
               [x (force x-thunk)]))))]))


#|

TODO fix transparent-lambda,
which should define a function that knows its own syntax.
Maybe need a struct-based model of pure-vanilla syntax first?

|#
(define-syntax-rule (transparent-lambda forms ...) (lambda forms ...))

#|

- no need to expand macros to discover a def form
- simply shallow-parse each form and collect lhs of defs
- provide all the ids
- force all the ids

|#
(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (with-syntax ([(defined-ids ...) (filter identifier?
                                              (for/list ([stx (syntax->list #'(forms ...))])
                                                (syntax-case stx ()
                                                  [(head lhs body) (and (identifier? #'head)
                                                                        (free-identifier=? #'head
                                                                                           #'define/thunk))
                                                   #'lhs]
                                                  [_ #false])))])
       #'(#%module-begin
          (provide defined-ids ...)
          forms ...
          (let* ([ignore defined-ids] ...)
            (void))))]))

(define-syntax cond-no-void
  (syntax-rules (else)
    [(_ [lhs rhs0 rhs ...] ...
        [else rhs0-final rhs-final ...])
     (cond [lhs (begin rhs0 rhs ...)] ...
           [else (begin rhs0-final rhs-final ...)])]))
