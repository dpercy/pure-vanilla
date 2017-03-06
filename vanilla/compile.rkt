#lang racket

(provide compile)

(require racket/syntax)
(require syntax/srcloc)
(require (submod "./parse.rkt" ast))
(require (for-template "./runtime.rkt"))
(require (for-template (only-in racket/require path-up)))


(define (wrap-stx ast stx)
  (if (syntax? stx)
      (datum->syntax stx
                     (syntax-e stx)
                     (Syntax-loc ast)
                     ; 4th arg is prop: syntax properties.
                     ; this includes a special mark read by Check Syntax.
                     #'())
      stx))

(define (globals ast) ; -> setof noloc global
  (define r globals)
  (define U set-union)
  (match ast
    [(Global _ _ _) (set (noloc ast))]
    [(Def _ var expr) (U (r var) (r expr))]

    [(Program _ statements) (apply U (map r statements))]

    [(Lit _ value) (set)]
    [(Quote _ ast) (set)]
    [(Unresolved _ name) (set)]

    [(Local _ _ _) (set)]
    [(Func _ params body) (r body)]

    [(Call _ func args) (apply U (r func) (map r args))]
    [(If _ t c a) (apply U (map r (list t c a)))]))

(define (compile ast mod-name) ; -> syntax-object or list of syntax-object
  (define (r ast) (compile ast mod-name))
  (wrap-stx
   ast
   (match ast
     ; each def actually creates a thunk,
     ; and a Global expression forces it.
     [(Global _ _ _) #`(force #,(compile-id ast))]
     [(Def _ var expr) #`(begin
                           (define #,(compile-id var) (delay #,(r expr))))]

     [(Program _ statements) (append
                              ; provide
                              (for/list ([s statements]
                                         #:when (Def? s)
                                         [v (in-value (Def-var s))])
                                #`(provide (rename-out [#,(compile-id v)
                                                        #,(Global-name v)])))
                              ; require
                              (list #`(require
                                       (only-in racket/require path-up)
                                       #,(datum->syntax #f 'vanilla/runtime)))
                              (for*/list ([g (in-set (globals ast))]
                                          [m (in-value (Global-mod g))]
                                          #:when (not (member m
                                                              (list #false
                                                                    'Base
                                                                    mod-name))))
                                #`(require (rename-in
                                            #,(list 'path-up (symbol->string m))
                                            [#,(Global-name g) #,(compile-id g)])))
                              ; definitions only
                              (for/list ([s statements]
                                         #:when (Def? s))
                                (r s))
                              ; force all the definitions, and run the expressions
                              (for/list ([s statements])
                                (match s
                                  [(Def _ var expr) #`(void #,(r var))]
                                  [_ (r s)])))]

     [(Lit _ value)  #`(quote #,value)]
     [(Quote _ ast)  #`(quote #,ast)]
     [(Unresolved _ name)  (error 'compile "Unresolved identifier: ~s" name)]

     ; locals can only refer to function parameters
     [(Local _ _ _) (compile-id ast)]
     [(Func _ params body)  #`(Function (lambda #,(map r params)
                                          #,(r body))
                                        #,(compile-template ast (set)))]


     [(Call _ func args)  #`(#%app #,(r func)
                                   #,@(map r args))]
     [(If _ test consq alt) #`(if (boolean=? #true #,(r test))
                                  #,(r consq)
                                  #,(r alt))])))

(define (compile-id ast)
  (match ast
    ; all generated identifiers have a dot.
    ; nice side effect: no conflict with Racket ids (quote, lambda, etc).
    [(Local _ name number)  (format-id #f "~a.~a" name number)]
    [(Global _ mod name)  (format-id #f "~a.~a" (or mod "") name)]))

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
