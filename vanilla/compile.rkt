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
     [(Program _ statements) (list*
                              (datum->syntax #f (list #'require 'vanilla/runtime))
                              (map compile statements))]
     ; TODO see ./macro-demos for how to do the correct evaluation order
     [(Def _ var expr) #`(begin
                           (provide (rename-out [#,(compile var)
                                                 #,(Global-name var)]))
                           (define #,(compile var) #,(compile expr)))]
     ; all generated identifiers have a dot.
     ; nice side effect: no conflict with Racket ids (quote, lambda, etc).
     [(Local _ name number)  (format-id #f "~a.~a" name number)]
     ;;[(Global _ mod name)  (format-id #f "~a.~a" (or mod "") name)]
     [(Global _ #f name)  (format-id #f ".~a" name)]
     [(Global _ 'Base name)  (format-id #f "Base.~a" name)]

     [(Lit _ value)  #`(quote #,value)]
     [(Unresolved _ name)  (error 'compile "Unresolved identifier: ~s" name)]

     [(Func _ params body)  #`(Function (lambda #,(map compile params)
                                          #,(compile body))
                                        #,(compile-template ast (set)))]
     [(Call _ func args)  #`(#%app #,(compile func)
                                   #,@(map compile args))]
     [(If _ test consq alt) #`(if (boolean=? #true #,(compile test))
                                  #,(compile consq)
                                  #,(compile alt))])))

(define (sl ast) ; strip loc from Local
  (match ast
    [(Local _ name number) (Local #f name number)]))
; compile-template "quotes" the ast:
; it produces a syntax-object that when evaluated returns that AST.
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

    [(Lit _ value)  #`(quote #,ast)]
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
