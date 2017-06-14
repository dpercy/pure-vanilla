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
; 1. impl simple pratt parser POC
; 2. add scope resolution
; 3. add symbol table / operators
; 4. add general Racket extensions

(tag-structs
 "Token"
 (NameDotName x y)
 (NameDotNumber x n)
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
        [#\" (string!)]
        [(? char-numeric?) (number!)]
        [#\( (begin (read-char port) (Open))]
        [#\) (begin (read-char port) (Close))]
        [#\, (begin (read-char port) (Comma))]
        [#\-  (arrow!)])))

  (define (name-start? c) (or (equal? c #\_)
                              (char-alphabetic? c)))
  (define (name-char? c) (or (name-start? c)
                             (char-numeric? c)))
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
               [c (error 'lex "Unexpected character after dot: ~v" c)]))]
      [_ (keyword-or-bare-name x)]))

  (define (keyword-or-bare-name name)
    (match name
      ["fn" (Fn)]
      [_ (error 'lex "TODO: bare names: ~v" name)]))

  ; NOTE this is cheating: using Racket's lexer.
  (define (string!) (String (read port)))

  (define (number!) (Number (string->number (eat-while! char-numeric?))))

  (define (arrow!)
    (match (peek-char port)
      [#\- (begin
             (read-char port)
             (match (peek-char port)
               [#\> (begin
                      (read-char port)
                      (Arrow))]
               [c (error 'lex "Unexpected char: ~v" c)]))]
      [c (error 'lex "Unexpected char: ~v" c)]))

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

  ;;
  )



(define (parse lex!)
  ; wrap the lexer in a box to cache the current token
  (define curtok (box (lex!)))
  (define (peek) (unbox curtok))
  (define (advance!) (set-box! curtok (lex!)))

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
  (define (parse-expression)
    (match (peek)
      [(NameDotName mod name) (begin (advance!) (Global mod name))]
      [(NameDotNumber name number) (begin (advance!) (Local name number))]
      [(String s) (begin (advance!) (Lit s))]
      [(Number n) (begin (advance!) (Lit n))]

      [(Fn) (parse-function)]

      ; TODO infix rule for "(" for Call

      [tok (error 'parse "Unexpected token ~v" tok)]))

  (define (parse-function)
    (expect! (Fn))
    (define params (parse-parameters))
    (expect! (Arrow))
    (define body (parse-expression))
    (Func params body))

  (define (parse-parameters)
    (expect! (Open))
    (parse-sep-end parse-parameter (Comma) (Close)))

  (define (parse-parameter)
    (match (peek)
      [(NameDotNumber name number) (begin (advance!) (Local name number))]
      [c (error 'parse "Expected a parameter but got ~v" c)]))

  (parse-expression))
(module+ test
  (require rackunit)

  (define (p s)
    (with-handlers ([values values])
      (parse (make-lexer (open-input-string s)))))

  (check-equal? (p "Foo.bar") (Global "Foo" "bar"))
  (check-equal? (p "bar.42") (Local "bar" 42))
  (check-equal? (p "\"zxcv\"") (Lit "zxcv"))
  (check-equal? (p "1234") (Lit 1234))

  (check-equal? (p "fn() -> 1") (Func '() (Lit 1)))
  (check-equal? (p "fn(x.0) -> 1") (Func (list (Local "x" 0)) (Lit 1)))
  (check-equal? (p "fn(x.0,) -> 1") (Func (list (Local "x" 0)) (Lit 1)))
  (check-equal? (p "fn(x.0,x.1) -> 1") (Func (list (Local "x" 0) (Local "x" 1)) (Lit 1)))
  (check-equal? (p "fn(x.0,x.1,) -> 1") (Func (list (Local "x" 0) (Local "x" 1)) (Lit 1)))

  #|
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
  |#

  ; right recursion
  (check-equal? (p "fn() -> fn(x.0, y.0) -> fn(x.1) -> 1")
                (Func '()
                      (Func (list (Local "x" 0) (Local "y" 0))
                            (Func (list (Local "x" 1))
                                  (Lit 1)))))

  #|
  ; call binds tighter than func
  (check-equal? (p "fn() -> 1()") (Func '() (Call (Lit 1) '())))
  |#


  ;;
  )
