#lang racket

#|

Extensible, scope-sensitive parser experiment

|#

(define-values {ast? ast-child?}
  (flat-murec-contract ([ast? (or/c symbol?
                                    (cons/c symbol? (listof ast-child?)))]
                        [ast-child? (or/c symbol? string? ast? (listof ast-child?))])
                       (values ast? ast-child?)))


#|

A rule is a sequence of patterns like
`in` means a literal keyword
`foo:expression` means any expression;
similarly, `foo:whatever` refers to that other syntax class
`

|#

(struct Repeat (subpat ; pattern
                delim  ; string
                ) #:transparent)
(struct Hole (name ; symbol
              class ; symbol
              ) #:transparent)

; each pattern corresponds to an ast-child
(define Pattern?
  (or/c Repeat? ; listof ast-child
        symbol? ; symbol
        string? ; string
        Hole?   ; ast
        ))

(struct Rule (tag patterns coverings) #:transparent)
(struct Covers (definitions ; hole-name
                 uses ; listof hole-name
                 ) #:transparent)

(struct Grammar (table ; map( symbol -> listof( Rule ) )
                 ) #:transparent)


(struct Result (ast tokens) #:transparent)

(define current-grammar (make-parameter #f))
(define current-tokens (make-parameter #f)) ; listof any/c
(define (peek!)
  (match (current-tokens)
    ['() #f]
    [(cons tok _) tok]))
(define (get!)
  (match (current-tokens)
    ['() (fail! "unexpected EOF")]
    [(cons tok toks*) (begin
                        (current-tokens toks*)
                        tok)]))
(struct exn:fail:unrecoverable-parse-error exn:fail ())
(define (fail! msg)
  (raise (exn:fail:unrecoverable-parse-error
          (format "failed: ~s: with remaining input ~v"
                  msg
                  (current-tokens))
          (current-continuation-marks))))

(define/contract (parse nt) (-> symbol? (or/c #f ast?))
  (match nt
    ; some nonterminals are built-in and special
    ['id (if (symbol? (peek!))
             (get!)
             #false)]
    ['binder
     ; TODO remember that this is a binder
     (if (symbol? (peek!))
         (get!)
         #false)]
    [_
     (define rules (hash-ref (Grammar-table (current-grammar)) nt))
     (match (peek!)
       [#f #f]
       [tok
        (match (for/first ([rule rules]
                           #:when (equal? tok (Rule-tag rule)))
                 rule)
          ; no case of this nonterminal matched
          [#f #f]
          [(Rule _ patterns coverings)
           (get!)
           (cons tok (parse-patterns patterns))])])]))

(define/contract (parse-patterns patterns) (-> (listof Pattern?)
                                               (listof ast-child?))
  (for*/list ([p patterns]
              [v (in-value (match (parse-pattern p)
                             [#f (fail!
                                  (format "expected ~v" p))]
                             [v v]))]
              #:when (not (or (string? p)
                              (symbol? p))))
    v))
(define/contract (parse-pattern pattern) (-> Pattern?
                                             (or/c #f ast-child?))
  ; if it fails, it must consume no input
  (match pattern
    [(? (or/c symbol? string?)) (if (equal? pattern (peek!))
                                    (get!)
                                    #false)]
    [(Repeat subpat delim-str)
     ; now, there might be a valid subpat at the input,
     ; or there might be zero of them.
     (match (parse-pattern subpat)
       [#f '()]
       [v
        ; now, there might be a delimiter or there might not.
        (match (parse-pattern delim-str)
          ; no delimiter means the repeat is done
          [#f (list v)]
          ; delimiter means continue
          [_ (cons v (parse-pattern pattern))])])]
    [(Hole name class)
     (parse class)]))


(module+ test
  (require rackunit)

  ; expression := x | y | z | call c:expression(a:expression, ...)
  (define A (Grammar (hash 'expression (list (Rule 'x '() '())
                                             (Rule 'y '() '())
                                             (Rule 'z '() '())
                                             (Rule 'call (list (Hole 'c 'expression)
                                                               "("
                                                               (Repeat (Hole 'a 'expression)
                                                                       ",")
                                                               ")")
                                                   '())))))

  (define (p toks)
    (displayln (format "case ~v" toks))
    (parameterize ([current-tokens toks])
      (parse 'expression)))

  (current-grammar A)

  ; var case
  (check-equal? (p '(x))  '(x))
  ; junk left over
  (check-equal? (p '(x y z))  '(x))
  ; call case
  (check-equal? (p '(call x "(" ")"))  '(call (x) []))
  ; call in arg
  (check-equal? (p '(call x "(" call y "(" ")" ")"))  '(call (x) [(call (y) [])]))
  (check-equal? (p '(call x "(" y "," z ")"))  '(call (x) [(y) (z)]))
  (check-equal? (p '(call call x "(" y ")" "(" z ")"))  '(call (call (x) [(y)]) [(z)]))

  ; expression :=
  ; ; TODO allow a rule to be just an id
  ; | var x:id
  ; | app f:expression a:expression
  ; | lam x:binder e:expression   where  x binds e
  (define S (Grammar (hash 'expression
                           (list (Rule 'var (list (Hole 'x 'id)) '())
                                 (Rule 'app (list (Hole 'f 'expression) (Hole 'a 'expression)) '())
                                 (Rule 'lam
                                       (list (Hole 'x 'binder) (Hole 'e 'expression))
                                       (list (Covers 'x '(e))))))))

  (current-grammar S)

  (check-equal? (p '(var x)) '(var x))
  (check-equal? (p '(call var x var y)) '(call (var x) (var y)))

  ; in this demo, binders are replaced with a counter
  (check-equal? (p '(lam x var x)) '(lam 0 (var 0)))
  (check-equal? (p '(lam x lam y var x)) '(lam 0 (lam 1 (var 0))))
  (check-equal? (p '(lam x lam y var y)) '(lam 0 (lam 1 (var 1))))
  (check-equal? (p '(call lam x var x lam x var x))
                '(call (lam 0 (var 0))
                       (lam 1 (var 1))))


  ;;
  )
