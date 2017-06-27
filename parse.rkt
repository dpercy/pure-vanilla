#lang racket

(require "constructor.rkt"
         "ast.rkt")


#|

core grammar:

Mod.name  -- global
x.0       -- local
"asdf"    -- literal string
1234      -- literal natural

fn ( x.0 , ... ) -> E   -- function

E ( E , ... )           -- call



|#

; TODO:
; 1. (DONE) impl simple pratt parser POC
; 2. (DONE) add scope resolution
; 3. (DONE) add symbol table / operators
; 4. (DONE) add general Racket extensions
;  - don't do Call for infix ops - run a parselet
;  - test case can create new nodes to test parselet
;

(tag-structs
 "Token"
 (NameDotName x y)
 (NameDotNumber x n)
 (NameDotOp x y)
 (NameUnqualified x)
 (OpUnqualified x)
 (String s)
 (Number n)

 (Fn)
 (Open)
 (Close)
 (Comma)
 (Arrow)

 ;;
 )
(define (make-lexer port)
  ; Every time lex is called, returns another token from the input port.
  ; The only characters consumed from the port are part of the token.

  (define (lex!)
    (parameterize ([current-input-port
                    (make-input-port "not supposed to use this"
                                     (lambda _ (error 'read "not supposed to use this"))
                                     (lambda _ (error 'peek "not supposed to use this"))
                                     (lambda _ (error 'close "not supposed to use this")))])
      (eat-while! char-blank?)
      (match (peek-char port)
        [(? eof-object?) eof]
        [(? name-start?) (name!)]
        [(? op-char?) (op!)]
        [#\" (string!)]
        [(? char-numeric?) (number!)]
        [#\( (begin (read-char port) (Open))]
        [#\) (begin (read-char port) (Close))]
        [#\, (begin (read-char port) (Comma))])))

  (define (name-start? c) (or (equal? c #\_)
                              (char-alphabetic? c)))
  (define (name-char? c) (or (name-start? c)
                             (char-numeric? c)))
  (define (op-char? c) (and (or (char-symbolic? c)
                                (char-punctuation? c))
                            (not (member c '(#\" #\( #\) #\,)))))
  (define (name!)
    (define x (eat-while! name-char?))
    (match (peek-char port)
      [#\. (begin
             (read-char port)
             (match (peek-char port)
               [(? eof-object?) (error 'lex "Unexpected EOF after dot")]
               [(? name-start?) (begin
                                  (define y (eat-while! name-char?))
                                  (NameDotName x y))]
               [(? char-numeric?) (begin
                                    (define n (string->number (eat-while! char-numeric?)))
                                    (NameDotNumber x n))]
               [(? op-char?) (begin
                               (define y (eat-while! op-char?))
                               (NameDotOp x y))]
               [c (error 'lex "Unexpected character after dot: ~v" c)]))]
      [_ (keyword-or-bare-name x)]))

  (define (keyword-or-bare-name name)
    (match name
      ["fn" (Fn)]
      [_ (NameUnqualified name)]))

  (define (op!)
    (match (eat-while! op-char?)
      ["->" (Arrow)]
      [v (OpUnqualified v)]))

  ; NOTE this is cheating: using Racket's lexer.
  (define (string!) (String (read port)))

  (define (number!) (Number (string->number (eat-while! char-numeric?))))

  (define (eat-while! char-pred)
    (with-output-to-string
      (lambda ()
        (let loop ()
          (match (peek-char port)
            [(? eof-object?) (void)]
            [(? char-pred c) (begin
                               (read-char port)
                               (write-char c)
                               (loop))]
            [_ (void)])))))

  lex!)
(module+ test
  (require rackunit)

  (define (lex s)
    (for/list ([token (in-producer (make-lexer (open-input-string s))
                                   eof-object?)])
      token))

  (check-equal? (lex "asdf.qwer") (list (NameDotName "asdf" "qwer")))
  (check-equal? (lex "asdf.+*(") (list (NameDotOp "asdf" "+*") (Open)))

  ;;
  )



(define (parse lex! module-name infix-dispatch how-to-group)
  ; wrap the lexer in a box to cache the current token
  (define curtok (box (lex!)))
  (define (peek) (unbox curtok))
  (define (advance!) (set-box! curtok (lex!)))
  ; keep environment info in parameters
  (define bare-locals (make-parameter (set))) ; setof string

  ; conveniences for common parsing patterns
  (define (peek? pred-or-tok)
    (if (procedure? pred-or-tok)
        (pred-or-tok (peek))
        (equal? pred-or-tok (peek))))
  (define (expect! pred-or-tok)
    (if (peek? pred-or-tok)
        (advance!)
        (error 'parse "Expected ~v but got ~v" pred-or-tok (peek))))
  (define (parse-sep-end parse-item sep end)
    (cond
      [(peek? end) (begin (advance!) '())]
      [else
       (let ([first (parse-item)])
         (cond
           [(peek? end) (begin (advance!) (list first))]
           [(peek? sep) (begin (advance!) (cons first (parse-sep-end parse-item sep end)))]
           [else (error 'parse "Expected ~v or ~v but got ~v" sep end (peek))]))]))

  ; entry point for parsing
  (define (parse-expression
           #:higher-precedence-than [ops '()])
    (define e (match (peek)
                [(NameDotName mod name) (begin (advance!) (Global mod name))]
                [(NameDotNumber name number) (begin (advance!) (Local name number))]
                [(NameUnqualified name) (begin (advance!)
                                               (if (set-member? (bare-locals) name)
                                                   (Local name #false)
                                                   (Global module-name name)))]
                [(String s) (begin (advance!) (Lit s))]
                [(Number n) (begin (advance!) (Lit n))]

                [(Fn) (parse-function)]

                [(Open) (parse-block)]

                [tok (error 'parse "Unexpected token ~v" tok)]))
    (parse-infix e #:higher-precedence-than ops))

  ; given that we've already parsed lhs from the stream,
  ; check whether there is an infix operator and return either:
  ;  - just lhs
  ;  - lhs op rhs
  (define (parse-infix lhs
                       #:higher-precedence-than [ops '()])
    (define (cleanup tok)
      (match tok
        [(OpUnqualified name)   (NameDotOp   module-name name)]
        [(NameUnqualified name) #:when (not (set-member? (bare-locals) name))
         (NameDotName module-name name)]
        [_ tok]))
    (match (peek)
      [(Open)
       (let ()
         (advance!)
         (define args (parse-sep-end parse-expression (Comma) (Close)))
         (define call (Call lhs args))
         (parse-infix call #:higher-precedence-than ops))]
      [(app cleanup (and (or (NameDotOp   mod name)
                             (NameDotName mod name))
                         tok))
       #:when (let ([op (Global mod name)])
                (match (infix-dispatch op)
                  ['illegal (error 'parse "Not defined as an infix operator: ~v" op)]
                  [(? procedure?)
                   (for/and ([other ops])
                     (match (how-to-group other op)
                       ; The left operator binds tighter, so just return.
                       ['left #false]
                       ; The right operator binds tighter;
                       ; if it binds tigher than all operators in the context, we can
                       ; keep parsing with right-recursion.
                       ['right #true]
                       ['illegal
                        (error 'parse
                               "No precedence defined for ~v and ~v; use parentheses to disambiguate"
                               other op)]
                       [v (error 'parse "how-to-group returned an invalid value: ~v" v)]))]))
       (let ()
         (define op (Global mod name))
         (define parselet (infix-dispatch op))
         (advance!)
         ; The parselet can call parse-expression, which in turns calls
         ; parse-infix again. This is how right-recursion works.
         (define parselet-result
           (parselet lhs op peek advance!
                     (lambda () (parse-expression #:higher-precedence-than (cons op ops)))))
         ; Now this whole call becomes the left-hand side:
         ; there may be another infix operator.
         ; This is how left-recursion works.
         (parse-infix parselet-result #:higher-precedence-than ops))]
      [_ lhs]))

  (define (bare-local? lcl)
    (match lcl
      [(Local name #false) name]
      [_ #false]))

  (define (parse-function)
    (expect! (Fn))
    (define params (parse-parameters))
    ; TODO check duplicate params? or in a separate pass? or in the constructor?
    (expect! (Arrow))
    (define body (let ([bare-params (filter-map bare-local? params)])
                   (parameterize ([bare-locals (for/fold ([s (bare-locals)])
                                                         ([p bare-params])
                                                 (set-add s p))])
                     (parse-expression))))
    (Func params body))

  (define (parse-parameters)
    (expect! (Open))
    (parse-sep-end parse-parameter (Comma) (Close)))

  (define (parse-parameter)
    (match (peek)
      [(NameDotNumber name number) (begin (advance!) (Local name number))]
      [(NameUnqualified name) (begin (advance!) (Local name #false))]
      [c (error 'parse "Expected a parameter but got ~v" c)]))

  (define (parse-block)
    (expect! (Open))
    (define e (parse-expression))
    (expect! (Close))
    e)

  (define v (parse-expression))
  (expect! eof-object?)
  v)
(module+ test
  (require rackunit)

  (define (infix-dispatch tok)
    (match tok
      [(Global "M" (or "^" "*" "+" "-" "$" "&")) parselet:infix-call]
      [(Global "M" "and") parselet:and]
      [(Global "M" "or") parselet:or]
      [_ 'illegal]))

  (define (how-to-group a b)
    (match* {a b}
      ; Parse does not compute transitive cases from this function.
      ; This function must be explicit in every case.
      [{(Global "M" "*") (Global "M" "+")} 'left]
      [{(Global "M" "+") (Global "M" "*")} 'right]

      [{(Global "M" "^") (Global "M" "*")} 'left]
      [{(Global "M" "*") (Global "M" "^")} 'right]

      [{(Global "M" "^") (Global "M" "+")} 'left]
      [{(Global "M" "+") (Global "M" "^")} 'right]

      ; Technically I should have 4 more cases with "-", but I'm not testing those cases.

      ; Cases for "+" and "-" chaining
      [{(Global "M" "+") (Global "M" "+")} 'left]
      [{(Global "M" "-") (Global "M" "-")} 'left]
      [{(Global "M" "+") (Global "M" "-")} 'left]
      [{(Global "M" "-") (Global "M" "+")} 'left]

      ; Example right-associative operator
      [{(Global "M" "$") (Global "M" "$")} 'right]

      ; "and" binds tighter than "or"
      [{(Global "M" "and") (Global "M" "or")} 'left]
      [{(Global "M" "or") (Global "M" "and")} 'right]

      ; If no rules cover this pair of operators, user must disambiguate.
      [{_ _} 'illegal]))

  (define (parselet:infix-call lhs op peek advance! parse-expression)
    (define rhs (parse-expression))
    (Call op (list lhs rhs)))

  (tag-structs
   "Logic"
   (And x y)
   (Or x y))
  (define (parselet:and lhs op peek advance! parse-expression)
    (define rhs (parse-expression))
    (And lhs rhs))
  (define (parselet:or lhs op peek advance! parse-expression)
    (define rhs (parse-expression))
    (Or lhs rhs))

  (define (p s)
    (parse (make-lexer (open-input-string s))
           "M"
           infix-dispatch
           how-to-group))

  (check-equal? (p "Foo.bar") (Global "Foo" "bar"))
  (check-equal? (p "bar.42") (Local "bar" 42))
  (check-equal? (p "\"zxcv\"") (Lit "zxcv"))
  (check-equal? (p "1234") (Lit 1234))

  (check-equal? (p "fn() -> 1") (Func '() (Lit 1)))
  (check-equal? (p "fn(x.0) -> 1") (Func (list (Local "x" 0)) (Lit 1)))
  (check-equal? (p "fn(x.0,) -> 1") (Func (list (Local "x" 0)) (Lit 1)))
  (check-equal? (p "fn(x.0,x.1) -> 1") (Func (list (Local "x" 0) (Local "x" 1)) (Lit 1)))
  (check-equal? (p "fn(x.0,x.1,) -> 1") (Func (list (Local "x" 0) (Local "x" 1)) (Lit 1)))

  ; scope resolution
  (check-equal? (p "x") (p "M.x"))
  (check-equal? (p "fn(x) -> 1") (Func (list (Local "x" #f)) (Lit 1)))
  (check-equal? (p "fn(x) -> x") (Func (list (Local "x" #f)) (Local "x" #f)))
  (check-equal? (p "fn(x.0) -> x") (p "fn(x.0) -> M.x")) ; x.0 doesn't bind x
  (check-equal? (p "fn(x) -> x.0") (Func (list (Local "x" #f)) (Local "x" 0)))

  ; binops
  (define (op name left right)
    (Call (Global "M" name) (list left right)))
  (check-equal? (p "1 M.+ 2") (op "+" (Lit 1) (Lit 2)))
  (check-equal? (p "1 M.* 2") (op "*" (Lit 1) (Lit 2)))
  (check-equal? (p "1 M.& 2") (op "&" (Lit 1) (Lit 2)))
  ; precedence
  ; - left recursive
  (check-equal? (p "1 M.* 2 M.+ 3") (op "+" (op "*" (Lit 1) (Lit 2)) (Lit 3)))
  ; - right recursive
  (check-equal? (p "1 M.+ 2 M.* 3") (op "+" (Lit 1) (op "*" (Lit 2) (Lit 3))))
  ; - both
  (check-equal? (p "1 M.+ 2 M.* 3 M.^ 4") (op "+" (Lit 1) (op "*" (Lit 2) (op "^" (Lit 3) (Lit 4)))))
  (check-equal? (p "1 M.+ 2 M.^ 3 M.* 4") (op "+" (Lit 1) (op "*" (op "^" (Lit 2) (Lit 3)) (Lit 4))))
  (check-equal? (p "1 M.* 2 M.^ 3 M.+ 4") (op "+" (op "*" (Lit 1) (op "^" (Lit 2) (Lit 3))) (Lit 4)))

  ; binop scope resolution
  (check-equal? (p "1 * 2 + 3") (op "+" (op "*" (Lit 1) (Lit 2)) (Lit 3)))

  ; undefined precedence
  (check-exn exn:fail?
             (lambda () (p "1 M.& 2 M.+ 3"))
             "use parentheses to disambiguate")
  (check-equal? (p "(1 M.& 2) M.+ 3") (op "+" (op "&" (Lit 1) (Lit 2)) (Lit 3)))
  (check-equal? (p "1 M.& (2 M.+ 3)") (op "&" (Lit 1) (op "+" (Lit 2) (Lit 3))))

  ; associative operators
  (check-equal? (p "1 M.+ 2 M.+ 3 M.+ 4") (p "((1 M.+ 2) M.+ 3) M.+ 4"))
  (check-equal? (p "1 M.+ 2 M.- 3 M.+ 4") (p "((1 M.+ 2) M.- 3) M.+ 4"))
  (check-equal? (p "1 M.- 2 M.- 3 M.+ 4") (p "((1 M.- 2) M.- 3) M.+ 4"))
  (check-equal? (p "1 M.+ 2 M.+ 3 M.- 4") (p "((1 M.+ 2) M.+ 3) M.- 4"))

  (check-equal? (p "1 M.$ 2 M.$ 3 M.$ 4") (p "1 M.$ (2 M.$ (3 M.$ 4))"))
  (check-exn exn:fail?
             (lambda () (p "1 M.$ 2 M.$ 3 M.+ 4"))
             "use parentheses to disambiguate")

  (check-equal? (p "1()") (Call (Lit 1) '()))
  (check-equal? (p "1(2)") (Call (Lit 1) (list (Lit 2))))
  (check-equal? (p "1(2,)") (Call (Lit 1) (list (Lit 2))))
  (check-equal? (p "1(2,3)") (Call (Lit 1) (list (Lit 2) (Lit 3))))
  (check-equal? (p "1(2,3,)") (Call (Lit 1) (list (Lit 2) (Lit 3))))

  ; left recursion
  (check-equal? (p "1()(2,3)(4)")
                (Call (Call (Call (Lit 1)
                                  '())
                            (list (Lit 2) (Lit 3)))
                      (list (Lit 4))))

  ; right recursion
  (check-equal? (p "fn() -> fn(x.0, y.0) -> fn(x.1) -> 1")
                (Func '()
                      (Func (list (Local "x" 0) (Local "y" 0))
                            (Func (list (Local "x" 1))
                                  (Lit 1)))))

  ; call binds tighter than func
  (check-equal? (p "fn() -> 1()") (Func '() (Call (Lit 1) '())))
  ; call binds tighter than binop
  (check-equal? (p "1 M.+ 2()") (op "+" (Lit 1) (Call (Lit 2) '())))

  ; logical operator - returns a custom node type
  (check-equal? (p "1 and 2") (And (Lit 1) (Lit 2)))
  (check-equal? (p "1 or 2") (Or (Lit 1) (Lit 2)))
  (check-equal? (p "1 or 2 and 3") (Or (Lit 1) (And (Lit 2) (Lit 3))))
  (check-equal? (p "1 and 2 or 3") (Or (And (Lit 1) (Lit 2)) (Lit 3)))

  ; shadowing keywords works fine
  (check-equal? (p "fn(and) -> and") (Func (list (Local "and" #f)) (Local "and" #f)))

  ;;
  )
