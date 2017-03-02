#lang racket

(provide (all-defined-out))

(require (submod "parse.rkt" ast)
         "parse.rkt")

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


;errors
(define Base.error (Function error (Global #f 'Base 'error)))

; generic
(define Base.show (Function show (Global #f 'Base 'show)))
(define Base.== (Function equal? (Global #f 'Base '==)))
(define Base.length (Function
                     (match-lambda
                       [(? string? s) (string-length s)]
                       [lst (length lst)])
                     (Global #f 'Base 'length)))

; numbers
(define Base.+ (Function + (Global #f 'Base '+)))
(define Base.- (Function - (Global #f 'Base '-)))
(define Base.< (Function < (Global #f 'Base '<)))
(define Base.abs (Function abs (Global #f 'Base 'abs)))

; lists
(define Base.list (Function list (Global #f 'Base 'list)))
(define Base.isEmpty (Function empty? (Global #f 'Base 'isEmpty)))
(define Base.cons (Function cons (Global #f 'Base 'cons)))
(define Base.first (Function first (Global #f 'Base 'first)))
(define Base.rest (Function rest (Global #f 'Base 'rest)))
; - helpers
(define Base.map (Function map (Global #f 'Base 'map)))
(define Base.foldl (Function foldl-haskell-style (Global #f 'Base 'foldl)))
(define Base.concat (Function (curry apply append) (Global #f 'Base 'concat)))
(define Base.elem (Function
                   (lambda (x lst)
                     (not (false? (member x lst))))
                   (Global #f 'Base 'elem)))

; strings
(define Base.split (Function string-split (Global #f 'Base 'split)))
(define Base.slice (Function substring (Global #f 'Base 'slice)))
(define Base.replicate (Function make-list (Global #f 'Base 'replicate)))
(define Base.parseInt (Function string->number (Global #f 'Base 'parseInt)))
