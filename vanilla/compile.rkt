#lang racket

(provide compile)

(require racket/syntax)
(require syntax/srcloc)
(require (submod "./parse.rkt" ast))
(require (for-template "./runtime.rkt"))


(define (wrap-stx ast stx)
  (if (syntax? stx)
      (datum->syntax stx
                     (syntax-e stx)
                     (Syntax-loc ast)
                     ; 4th arg is prop: syntax properties.
                     ; this includes a special mark read by Check Syntax.
                     #'())
      stx))

(define (compile ast) ; -> syntax-object or list of syntax-object
  (wrap-stx
   ast
   (match ast
     ; each def actually creates a thunk,
     ; and a Global expression forces it.
     [(Global _ _ _) #`(force #,(compile-id ast))]
     [(Def _ var expr) #`(begin
                           (define #,(compile-id var) (delay #,(compile expr))))]

     [(Program _ statements) (append
                              ; provide
                              (for/list ([s statements]
                                         #:when (Def? s)
                                         [v (in-value (Def-var s))])
                                #`(provide (rename-out [#,(compile-id v)
                                                        #,(Global-name v)])))
                              ; require
                              (list
                               (datum->syntax #f
                                              (list #'require 'vanilla/runtime)))
                              (list #`(provide (rename-out
                                                #,@(for/list ([s statements]
                                                              #:when (Def? s))
                                                     (let ([v (Def-var s)])
                                                       (list (compile-id v)
                                                             (Global-name v))))))
                                    )
                              ; definitions and toplevel expressions
                              (map compile statements)
                              ; force all the definitions
                              (for/list ([s statements]
                                         #:when (Def? s)
                                         [v (in-value (Def-var s))])
                                #`(void #,(compile v))))]



     [(Lit _ value)  #`(quote #,value)]
     [(Quote _ ast)  #`(quote #,ast)]
     [(Unresolved _ name)  (error 'compile "Unresolved identifier: ~s" name)]

     ; locals can only refer to function parameters
     [(Local _ _ _) (compile-id ast)]
     [(Func _ params body)  #`(Function (lambda #,(map compile params)
                                          #,(compile body))
                                        #,(compile-template ast (set)))]


     [(Call _ func args)  #`(#%app #,(compile func)
                                   #,@(map compile args))]
     [(If _ test consq alt) #`(if (boolean=? #true #,(compile test))
                                  #,(compile consq)
                                  #,(compile alt))])))

(define (compile-id ast)
  (match ast
    ; all generated identifiers have a dot.
    ; nice side effect: no conflict with Racket ids (quote, lambda, etc).
    [(Local _ name number)  (format-id #f "~a.~a" name number)]
    ;;[(Global _ mod name)  (format-id #f "~a.~a" (or mod "") name)]
    [(Global _ #f name)  (format-id #f ".~a" name)]
    [(Global _ 'Base name)  (format-id #f "Base.~a" name)]
    ))

(define (sl ast) ; strip loc from Local
  (match ast
    [(Local _ name number) (Local #f name number)]))
; compile-template "quotes" the ast:
; it produces Racket code that when evaluated returns that AST.
; However, any free local variables in the AST are compiled to Racket variables.
; This allows currying like (Function-syntax "let x = 1 in () -> f(x)")
; to return "() -> f(1)".
(define (compile-template ast env) ; -> syntax-object
  ; env : set( Local )
  (match ast
    ; bound ids become symbols, as in :( x -> x )
    [(Local _ name number) #:when (set-member? env (sl ast))
     #`(quote #,ast)]
    ; unbound ids are holes in the template:
    ; they are compiled to variables so that :(  ()-> y ) will plug
    ; in the current value of y.
    [(Local _ name number) #`(value->syntax #,(format-id #f "~a.~a" name number))]

    [(Global _ mod name)  #`(quote #,ast)]

    ; lit and quote both don't really have free variables
    [(Lit _ _)    #`(quote #,ast)]
    [(Quote _ _)  #`(quote #,ast)]
    [(Unresolved loc name)  (raise-syntax-error name
                                                "Unresolved identifier"
                                                (build-source-location-syntax loc))]

    [(Func loc params body)  (let ([env (for/fold ([env env]) ([p params])
                                          (set-add env (sl p)))])
                               #`(Func (quote #,loc)
                                       (list #,@(for/list ([p params])
                                                  (compile-template p env)))
                                       #,(compile-template body env)))]
    [(Call loc func args)  #`(Call (quote #,loc)
                                   #,(compile-template func env)
                                   (list #,@(for/list ([a args])
                                              (compile-template a env))))]
    [(If loc test consq alt)  #`(If (quote #,loc)
                                    #,(compile-template test  env)
                                    #,(compile-template consq env)
                                    #,(compile-template alt   env))]))
