#lang racket

(provide (all-defined-out))

(require (submod "parse.rkt" ast)
         "parse.rkt")

(require racket/generic)

(define-generics friendly-equal?
  (friendly-equal-key friendly-equal?)
  #:defaults ([Syntax? (define (friendly-equal-key v)
                         (noloc v))]
              [any/c (define (friendly-equal-key v)
                       v)]))

(define (friendly-equal? a b)
  (equal? (friendly-equal-key a)
          (friendly-equal-key b)))

(struct Function (procedure syntax)
  ; TODO put a syntax-thunk here instead, to avoid constructing all the syntax nodes
  ; when they're not needed.
  #:property prop:procedure
  (struct-field-index procedure))

(define (value->syntax v)
  (match v
    [(Function _ syntax) syntax]
    [(? Syntax? ast) (Quote #f ast)]
    [(? list?) (Call #f (Global #f 'Base 'list)
                     (map value->syntax v))]
    [v (Lit #f v)]))


(define (base-exported-names)
  (define-values {exp-values exp-syntax} (module->exports 'vanilla/runtime))
  ; exp-values is an alist mapping phase-numbers to "exports".
  (define exports (rest (assoc 0 exp-values)))
  ; each "export" is (list/c symbol origin-list).
  (define (base-name sym) ; 'Base.foo -> 'foo
    (let ([str (symbol->string sym)])
      (and (string-prefix? str "Base.")
           (string->symbol (string-replace str "Base." "")))))
  (filter-map base-name (map first exports)))



(define (show v)
  (show-syntax (value->syntax v)))

(define (foldl-haskell-style f init lst)
  (match lst
    ['() init]
    [(cons x xs) (foldl-haskell-style f (f init x) xs)]))
(module+ test
  (require rackunit)

  ; Haskell-style foldl keeps arguments in the same order
  (check-equal? (foldl-haskell-style list #f '(1 2 3))
                '(((#f 1) 2) 3))

  ; Racket-style foldl puts the accumulator on the right
  (check-equal? (foldl list #f '(1 2 3))
                '(3 (2 (1 #f)))))

(struct tagged (tag contents) #:transparent)

(define (pack sym . args)
  (match sym
    [(Global _ m n) (tagged (Global #f m n) args)]))

(define (unpack sym v)
  (match sym
    [(Global _ m n)
     (match (as-tagged v)
       [(tagged (Global _ m* n*) contents)  (if (and (equal? m m*)
                                                     (equal? n n*))
                                                contents
                                                #false)]
       ; this case should never happen in the object language,
       ; but it could happen if unpack is misused in Racket.
       [_ (error 'unpack "not a tagged value: ~v" v)])]
    [_ (error 'unpack "not a global symbol: ~v" sym)]))

(define (as-tagged v)
  (match v
    [(? tagged? v) v]
    [(? Syntax? v)
     (define (t s . body)
       (tagged (Global #f 'Syntax s) body))
     (match v
       ; TODO weird that loc is hidden from inspection
       [(Program _ statements) (t 'Program statements)]
       [(Def _ var expr) (t 'Def var expr)]
       [(Lit _ value) (t 'Lit value)]
       [(Quote _ ast) (t 'Quote ast)]
       [(Local _ name num) (t 'Local (symbol->string name) num)]
       [(Global _ mod name) (t 'Global (symbol->string mod) (symbol->string name))]
       [(Unresolved _ name) (t 'Unresolved (symbol->string name))]
       [(Func _ params body) (t 'Func params body)]
       [(Call _ func args) (t 'Call func args)]
       [(If _ test consq alt) (t 'If test consq alt)])]
    [_ (error 'as-tagged "can't be expressed as a tagged value: ~v" v)]))

(define (make-variadic f)
  ; f takes a list
  ; return a function that takes N args
  (lambda args (f args)))

(define (get-siblings sym) ; list of ids
  (match sym
    [(Global _ mod _)
     ; TODO fix how module names work:
     ;  everything should be absolute within the "project" tree, like Java.
     (define module-name (symbol->string mod))
     ; load the module but don't execute its run-time code
     (dynamic-require module-name (void))
     (define-values {variables syntax} (module->exports module-name))
     ; variables is an alist mapping phase-numbers to "exports":
     ; each "export" is a list whose head is a symbol.
     (define exported-names (map first (rest (assoc 0 variables))))
     (for/list ([name exported-names])
       ; TODO include srcloc?
       (Global #f mod name))]
    [_ (error 'get-siblings "expected a Global")]))

(define (get-value sym) ; value
  (match sym
    [(Global _ mod name)
     ; TODO fix how module names work:
     ;  everything should be absolute within the "project" tree, like Java.
     (define module-name (symbol->string mod))
     ; load the module but don't execute its run-time code
     (dynamic-require module-name name)]
    [_ (error 'get-value "expected a Global")]))


; errors
(define Base.error (Function error (Global #f 'Base 'error)))

; generic
(define Base.show (Function show (Global #f 'Base 'show)))
(define Base.== (Function friendly-equal? (Global #f 'Base '==)))
(define Base.!= (Function (compose not friendly-equal?) (Global #f 'Base '!=)))
(define Base.=== (Function equal? (Global #f 'Base '===)))
(define Base.!== (Function (compose not equal?) (Global #f 'Base '!==)))
(define Base.length (Function
                     (match-lambda
                       [(? string? s) (string-length s)]
                       [lst (length lst)])
                     (Global #f 'Base 'length)))
(define Base.apply (Function apply (Global #f 'Base 'apply)))
(define Base.makeVariadic (Function make-variadic (Global #f 'Base 'makeVariadic)))

; booleans
(define Base.true #true)
(define Base.false #false)

; syntax
(define Base.pack (Function pack (Global #f 'Base 'pack)))
(define Base.unpack (Function unpack (Global #f 'Base 'unpack)))
(define Base.inspect (Function Function-syntax (Global #f 'Base 'inspect)))
(define Base.isSyntax (Function Syntax? (Global #f 'Base 'isSyntax)))
(define Base.getSiblings (Function get-siblings (Global #f 'Base 'getSiblings)))
(define Base.getValue (Function get-value (Global #f 'Base 'getValue)))

; numbers
(define Base.isNumber (Function number? (Global #f 'Base 'isNumber)))
(define Base.numerator (Function numerator (Global #f 'Base 'numerator)))
(define Base.denominator (Function denominator (Global #f 'Base 'denominator)))
(define Base.+ (Function + (Global #f 'Base '+)))
(define Base.- (Function - (Global #f 'Base '-)))
(define Base./ (Function / (Global #f 'Base '/)))
(define Base.< (Function < (Global #f 'Base '<)))

; lists
(define Base.list (Function list (Global #f 'Base 'list)))
(define Base.isEmpty (Function empty? (Global #f 'Base 'isEmpty)))
(define Base.isPair (Function cons? (Global #f 'Base 'isPair)))
(define Base.cons (Function cons (Global #f 'Base 'cons)))
(define Base.first (Function first (Global #f 'Base 'first)))
(define Base.rest (Function rest (Global #f 'Base 'rest)))
; - helpers
;   TODO port to prelude

; strings
(define Base.isString (Function string? (Global #f 'Base 'isString)))
(define Base.split (Function string-split (Global #f 'Base 'split)))
(define Base.slice (Function substring (Global #f 'Base 'slice)))
(define Base.parseInt (Function string->number (Global #f 'Base 'parseInt)))
(define Base.strcat (Function string-append (Global #f 'Base 'strcat)))

; functions
(define Base.isFunc (Function procedure? (Global #f 'Base 'isFunc)))
