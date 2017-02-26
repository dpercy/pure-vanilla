#lang racket

(provide (all-defined-out))

(require (submod "parse.rkt" ast))

(struct Function (procedure syntax)
  ; TODO put a syntax-thunk here instead, to avoid constructing all the syntax nodes
  ; when they're not needed.
  #:property prop:procedure
  (struct-field-index procedure))

(define (value->syntax v)
  (match v
    [(Function _ syntax) syntax]
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


(define Base.+ (Function + (Global #f 'Base '+)))
(define Base.- (Function - (Global #f 'Base '-)))
(define Base.< (Function < (Global #f 'Base '<)))
(define Base.== (Function equal? (Global #f 'Base '==)))
(define (show v)
  ;; TODO implement show in terms of value->syntax and show syntax.
  (format "~v" v))
(define Base.show (Function show (Global #f 'Base 'show)))

(define Base.list (Function list (Global #f 'Base 'list)))
(define Base.isEmpty (Function empty? (Global #f 'Base 'isEmpty)))
(define Base.cons (Function list (Global #f 'Base 'cons)))
(define Base.first (Function first (Global #f 'Base 'first)))
(define Base.rest (Function rest (Global #f 'Base 'rest)))

(define Base.error (Function error (Global #f 'Base 'error)))
