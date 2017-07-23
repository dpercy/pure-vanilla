#lang racket


#|

Idea:
- simple expression expander
- custom set of primitive forms
- check each ID as a prim before desugaring

core:
x
(quote v)
(lambda (x ...) e)
(e e ...)
(if e e e)



|#



#|

TODO hygiene
Hygiene involves renaming identifiers to avoid collisions.
See that Dybvig paper.
Try to keep it simple by putting hygiene info only on identifiers.
Try to make it transparent by printing all the hygiene info concisely.
For super-simplicity, implement "mark" by just appending to the symbol!

|#


(struct Expr () #:transparent)
(struct Quote Expr (value) #:transparent)
(struct Var Expr (name) #:transparent)
(struct Lambda Expr (param-names body) #:transparent)
(struct If Expr (test consq alt) #:transparent)
(struct Call Expr (func args) #:transparent)



(define self-quoting?
  (not/c (or/c symbol?
               empty?
               cons?
               void?)))


(define (core-prim-handler form fallback)
  (match form
    [(list 'var x) (Var x)]
    [(list 'quote v) (Quote v)]
    [(list 'lambda (? (listof symbol?) param-names) body) (Lambda param-names (parse body))]
    [(list* 'call func args) (Call (parse func) (map parse args))]
    [(list 'if t c a) (If (parse t) (parse c) (parse a))]
    [_ (fallback)]))

(define current-prim-handler (make-parameter core-prim-handler))

#|

parse drives the expander:
It's trying to convert the s-expression to something else.
When it hits a primitive form, it knows how to parse it.
Otherwise, it tries to call a macro.

|#
(define (parse form)
  ; 1. If the current prim handler knows how to compile this form, let it do so.
  ((current-prim-handler)
   form
   (lambda ()
     ; 2. Otherwise, try to reduce this form to something it can compile.
     (match form
       ; if the head is a primitive keyword, and the prim handler didn't compile it,
       ; it's an error of some kind:
       ; - maybe a syntax error
       ; - maybe a hole in the current prim handler
       [(cons (and head (or 'var 'quote 'lambda 'call 'if)) _)
        (error 'expand "~v: bad syntax: ~v" head form)]


       ; if head is a macro keyword, expand it
       [(cons 'and _) (parse (macro-and form))]
       [(cons 'let _) (parse (macro-let form))]

       ; if head is not a macro, desugar by adding one
       [(? self-quoting?) (parse (list 'quote form))]
       [(? symbol?)       (parse (list 'var   form))]
       [(cons _ _)        (parse (cons 'call form))]

       ['() (error 'expand "empty parens not allowed")]))))


; Pseudo-macros do all the error-checking that a macro would,
; but they can't expand to anything, because they represent primitives.
; They all return void, to remind you not to keep expanding the result.

(define (pseudo-macro-var form)
  (match form
    [(list 'var (? symbol?)) (void)]
    [_ (error 'expand "var: bad syntax: ~v" form)]))

(define (pseudo-macro-quote form)
  (match form
    [(list 'quote _) (void)]
    [_ (error 'expand "quote: bad syntax: ~v" form)]))

(define (pseudo-macro-lambda form)
  (match form
    [(list 'lambda (? (listof symbol?)) _) (void)]
    [_ (error 'expand "lambda: bad syntax: ~v" form)]))

(define (pseudo-macro-call form)
  (match form
    [(list* 'call func args) (void)]
    [_ (error 'expand "call: bad syntax: ~v" form)]))

(define (pseudo-macro-if form)
  (match form
    [(list 'if test consq alt) (void)]
    [_ (error 'expand "if: bad syntax: ~v" form)]))


(define (macro-and form)
  (match form
    [(list 'and) #true]
    [(list 'and x y) `(if ,x ,y #false)]
    [(list* 'and x rest) `(and x (and ,@rest))]))

(define (macro-let form)
  (match form
    [(list 'let (list (list lhs rhs) ...) body)
     `((lambda (,@lhs) ,body) ,@rhs)]))


(module+ test
  (require rackunit)

  ; example elaborating surface scheme to core scheme
  (check-equal? (parse '(let ([x 1]) (g (and x (f 4)))))
                (Call (Lambda '(x)
                              (Call (Var 'g)
                                    (list (If (Var 'x)
                                              (Call (Var 'f) (list (Quote 4)))
                                              (Quote #false)))))
                      (list (Quote 1))))

  ; elaborate to JS
  (define (js-prim-handler form fallback)
    (match form
      [(list 'var x) (symbol->string x)]
      [(list 'quote (? number? n)) (number->string n)]
      [(list 'lambda params body)
       (string-append "function("
                      (apply string-append
                             (add-between (map parse params)
                                          ", "))
                      ") { return "
                      (parse body)
                      "; }")]
      [(list* 'call func args)
       (string-append "("
                      (parse func)
                      ")("
                      (apply string-append
                             (add-between (map parse args)
                                          ", "))
                      ")")]
      [(list* 'and args)
       (string-append "("
                      (apply string-append
                             (add-between (map parse args)
                                          " && "))
                      ")")]
      [_ (fallback)]))
  (check-equal? (parameterize ([current-prim-handler js-prim-handler])
                  (parse '(let ([x 1]) (g (and x (f 4))))))
                "(function(x) { return (g)((x && (f)(4))); })(1)")

  (define (mongo-prim-handler form fallback)
    ; This prim handler tries to parse the form as a find(_) query:
    ; a predicate on documents.
    (match form
      ; The form has to be a 1-arg lambda.
      [(list 'lambda (list param) body)
       (parameterize ([current-prim-handler (doc-simple-predicate param)])
         (parse body))]
      [_ (fallback)]))
  (define ((doc-simple-predicate doc-iden) form fallback)
    (match form
      [(list 'call (and cmp (or 'equal? '>))
             (list 'hash-ref
                   (== doc-iden)
                   (list 'quote fieldname))
             (list 'quote val))
       (hash fieldname (hash (match cmp ['equal? '$eq] ['> '$gt])
                             val))]
      [_ (fallback)]))
  (parameterize ([current-prim-handler mongo-prim-handler])
    (check-equal? (parse '(lambda (doc) (equal? (hash-ref doc 'x) '"foo")))
                  (hash 'x (hash '$eq "foo")))

    ; TODO bug here
    ; This case is identical to the previous, except for removing the quote
    ; around "foo". This is basically saying the matcher doens't understand synonyms!
    ; Solutions:
    ;  - tree where every subtree remembers history
    ;  - match pattern that says "eventually expands to"
    ;     - step until subpattern succeeds; otherwise fail
    ;     - memoize to avoid redundant work
    ;     - may need to use the pattern to guide expansion:
    ;         - if the root already matches, stop expanding that part
    ;         i.e. (and (if 1 2 #f) 3 #f)
    ;         problem: expander doesn't know how to recur inside and


    #|


    Each subtree doesn't have a linear history:
    (print-twice x) -> (begin (displayln x) (displayln x))
    But each subtree in the output has at most one predecessor.

    #0=(and #1=(and x y) z)
    #2=(if #1=(and x y) z #f)
    #3=(if #4=(if x y #f) z #f)

    0 becomes 2
    2 becomes 3
    1 becomes 4

    match #0=(and #1=(and x y) z) (and (if _ _ _) _)
    unpack
    match #1=(and x y) (if _ _ _)
    1 becomes 4
    match #4=(if x y #f) (if _ _ _)



    |#
    (check-equal? (parse '(lambda (doc) (equal? (hash-ref doc 'x) "foo")))
                  (hash 'x (hash '$eq "foo")))



    (check-equal? (parse '(lambda (doc) (> (hash-ref doc 'x) '5)))
                  (hash 'x (hash '$gt 5)))
    (check-equal? (parse '(lambda (doc) (> (hash-ref doc 'x) 5)))
                  (hash 'x (hash '$gt 5)))
    )

  ;;
  )
