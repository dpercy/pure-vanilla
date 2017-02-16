#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)
(require "./util.rkt")


(define-tokens tokens (UnqualIden Num IdenNum QualIden))
(define-empty-tokens empty-tokens (EOF Equals Newline OpenParen CloseParen Arrow Comma))


(define-lex-abbrev iden (concatenation alphabetic
                                       (repetition 0 +inf.0 (union alphabetic numeric))))
(define-lex-abbrev nat (repetition 1 +inf.0 (char-range #\0 #\9)))
(define lex
  (lexer
   ; TODO implement comments as an actual token?
   ; whitespace with no newlines is ignored
   [(repetition 1 +inf.0
                (intersection whitespace
                              (char-complement #\newline)))  (lex input-port)]
   ; a newline plus any mixture of whitespace and newlines is a Newline token
   [(concatenation #\newline (repetition 0 +inf.0 whitespace))  (token-Newline)]
   [#\; (token-Newline)]

   [#\= (token-Equals)]

   [nat  (token-Num (string->number lexeme))]
   [(concatenation iden #\' nat) (match (string-split lexeme "'")
                                   [(list name num) (token-IdenNum (list (string->symbol name)
                                                                         (string->number num)))])]
   [(concatenation iden #\. iden)  (match (string-split lexeme ".")
                                     [(list qual name)
                                      (token-QualIden (list (string->symbol qual)
                                                            (string->symbol name)))])]
   [iden  (token-UnqualIden (string->symbol lexeme))]
   ["(" (token-OpenParen)]
   [")" (token-CloseParen)]
   ["->" (token-Arrow)]
   ["," (token-Comma)]


   [(eof)  (token-EOF)]))

(define (lex-string str)
  (let ([port (open-input-string str)])
    (sequence->list (in-producer (lambda () (curry lex port))
                                 (curry equal? (token-EOF))))))



(struct Program (statements) #:transparent)
(struct Def (var expr) #:transparent)

(struct Lit (value) #:transparent)
; `number` is not a De-Bruijn index or up-reference:
; it's an extension of the variable's name.
; Two locals are equal iff their `name` and `number` are equal.
(struct Local (name number) #:transparent)
(struct Global (mod name) #:transparent)
; An Unresolved expression doesn't properly belong in an Expr.
; It should never be observable by the user program.
(struct Unresolved (name) #:transparent)

(struct Func (params body) #:transparent)
(struct Call (func args) #:transparent)

(define pre-parse
  (parser
   (tokens tokens empty-tokens)
   (start Program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value)
            (error 'parse "Unexpected token ~v ~v" tok-name tok-value)))

   (precs (nonassoc Arrow)
          (nonassoc OpenParen)
          )
   (grammar
    (Program [(Statements) (Program $1)])
    (Statements [() (list)]
                [(Statement) (list $1)]
                [(Statement Newline Statements) (cons $1 $3)])
    (Statement [(Def) $1] [(Expr) $1])
    (Def [(Iden Equals Expr) (Def $1 $3)])
    (Iden [(UnqualIden) (Unresolved $1)]
          [(IdenNum) (match $1 [(list name num) (Local name num)])]
          [(QualIden) (match $1 [(list mod name) (Global mod name)])])
    (Expr [(ExprExceptIden) $1]
          [(Iden) $1])
    (ExprExceptIden [(Num) (Lit $1)]
                    [(Expr OpenParen Args CloseParen) (Call $1 $3)]

                    [(OpenParen ExprExceptIden CloseParen) $2]
                    [(OpenParen Iden CloseParen) $2]

                    ; several cases for function arrows
                    ; x -> 1
                    [(Iden Arrow Expr) (Func (list $1) $3)]
                    ; (x) -> 1
                    [(OpenParen Iden CloseParen Arrow Expr) (Func (list $2) $5)]
                    ; (x,) -> 1
                    [(OpenParen Iden Comma CloseParen Arrow Expr) (Func (list $2) $6)]
                    ; () -> 1
                    [(OpenParen CloseParen Arrow Expr) (Func '() $4)]
                    ; (x, y, ...) -> 1
                    [(OpenParen TwoOrMoreParams CloseParen Arrow Expr) (Func $2 $5)])
    (NonemptyParams [(Iden) (list $1)] ; base case with no trailing comma
                    [(Iden Comma) (list $1)] ; base case with trailing comma
                    [(Iden Comma NonemptyParams) (cons $1 $3)])
    (TwoOrMoreParams [(Iden Comma NonemptyParams) (cons $1 $3)])
    (Args [() (list)] ; empty case, or base case with trailing comma
          [(Expr) (list $1)] ; base case with no trailing comma
          [(Expr Comma Args) (cons $1 $3)]))))

(define (parse/imports lex! imports)
  (fix-scope-program (pre-parse lex!) (invert-hash-of-sets imports)))

(define (invert-hash-of-sets h)
  (for*/fold ([result (hash)]) ([{k s} (in-hash h)]
                                [v (in-set s)])
    (hash-set result v (set-add (hash-ref result v (set))
                                k))))
(module+ test
  (check-equal? (invert-hash-of-sets (hash 'm0 (set 'x 'f)
                                           'm1 (set 'x 'y)
                                           'm2 (set)))
                (hash 'x (set 'm0 'm1)
                      'f (set 'm0)
                      'y (set 'm1))))

(define (unwrap-unary-set s)
  (if (= 1 (set-count s))
      (set->list s)
      #false))

; imports-rev : hash( unqual-id-name -> set( module-name ) )
(define (fix-scope-program program imports-rev)
  (match program
    [(Program statements)
     ; local-env : name -> number, the max of in-scope locals
     (define local-env (hash))
     (define imports-rev*
       (for/fold ([imports-rev imports-rev]) ([stmt statements])
         (match stmt
           [(Def (Unresolved name) _) (hash-remove imports-rev name)]
           [_ imports-rev])))
     (Program (for/list ([stmt statements])
                (fix-scope-statement stmt imports-rev* local-env)))]))
(define (fix-scope-statement stmt imports-rev local-env)
  (match stmt
    [(Def var expr) (Def
                      (fix-scope-expr var imports-rev local-env)
                      (fix-scope-expr expr imports-rev local-env))]
    [expr (fix-scope-expr expr imports-rev local-env)]))
(define (fix-scope-expr expr imports-rev local-env)
  (define (recur expr) (fix-scope-expr expr imports-rev local-env))
  (match expr
    [(Local name number)  expr]
    [(Global mod name)  expr]


    [(Lit value)  expr]
    [(Unresolved name)  (match (hash-ref local-env name #f)
                          [(? number? n)  (Local name n)]
                          [#false

                           (match (hash-ref imports-rev name #f)
                             ; TODO should unbound unqual ID be an error?
                             ; IDs should refer to definitions!
                             [#false (Global #f name)]
                             [(app unwrap-unary-set (list m)) (Global m name)]
                             [choices (error 'fix-scope
                                             "Ambiguous reference ~a: could refer to ~a"
                                             name
                                             (string-join (for/list ([m choices])
                                                            (format "~s.~s" m name))
                                                          " or "))])])]

    [(Func params body)
     (let ([local-env* (for/fold ([local-env local-env]) ([p params])
                         (match p
                           [(Local _ _) local-env]
                           [(Unresolved name)
                            ; What's the new identifier number?
                            ; It should be 1 higher than the highest already in scope,
                            ; or 0 if none are in scope.
                            (define num (+ 1 (hash-ref local-env name -1)))
                            ; But also, it must not be used as a parameter anywhere in body.
                            (while (set-member? (explicit-locals body)
                                                (Local name num))
                              (set! num (+ num 1)))
                            (hash-set local-env name num)]))])
       (Func (for/list ([p params])
               (fix-scope-expr p imports-rev local-env*))
             (fix-scope-expr body imports-rev local-env*)))]
    [(Call func args)  (Call (recur func)
                             (map recur args))]))

(define explicit-locals ; expr -> set( Local )
  (memoize
   (make-weak-hasheq)
   (lambda (expr)
     (match expr
       [(Local name number)  (set expr)]
       [(Global mod name)  (set)]


       [(Lit value)  (set)]
       [(Unresolved name)  (set)]

       [(Func params body) (apply set-union
                                  (map explicit-locals (cons body params)))]
       [(Call func args)  (apply set-union
                                 (map explicit-locals (cons func args)))]))))

(define (parse-string/imports str imports-rev)
  (let* ([port (open-input-string str)]
         [lex! (lambda () (lex port))])
    (parse/imports lex! imports-rev)))
(define (parse-string str)
  (parse-string/imports str (hash)))
(module+ test
  (require rackunit)

  (check-equal? (parse-string "f(x, 1)")
                (Program (list (Call (Global #f 'f) (list (Global #f 'x)
                                                          (Lit 1))))))
  (check-equal? (parse-string "f = () -> 4")
                (Program (list (Def (Global #f 'f) (Func '() (Lit 4))))))

  ; application binds tighter than abstraction
  (check-equal? (parse-string "() -> 1")
                (Program (list (Func '() (Lit 1)))))

  ; parenthesized expr vs function
  (check-equal? (parse-string "(x)") (Program (list (Global #f 'x))))
  (check-equal? (parse-string "(x) -> 3") (Program (list (Func (list (Local 'x 0))
                                                               (Lit 3)))))

  ; function arity
  (check-equal? (parse-string "(x,) -> 3") (Program (list (Func (list (Local 'x 0))
                                                                (Lit 3)))))
  (check-equal? (parse-string "(x,y) -> 3") (Program (list (Func (list (Local 'x 0)
                                                                       (Local 'y 0))
                                                                 (Lit 3)))))
  (check-equal? (parse-string "(x,y,) -> 3") (Program (list (Func (list (Local 'x 0)
                                                                        (Local 'y 0))
                                                                  (Lit 3)))))

  ; expr nesting inside parens
  (check-equal? (parse-string "((x))") (Program (list (Global #f 'x))))
  (check-equal? (parse-string "(((x)))") (Program (list (Global #f 'x))))
  (check-equal? (parse-string "(x -> 1)") (Program (list (Func (list (Local 'x 0))
                                                               (Lit 1)))))

  ; scope and shadowing
  (check-equal? (parse-string "x -> x")
                (Program (list (Func (list (Local 'x 0))
                                     (Local 'x 0)))))
  (check-equal? (parse-string "x -> x -> x")
                (Program (list (Func (list (Local 'x 0))
                                     (Func (list (Local 'x 1))
                                           (Local 'x 1))))))
  (check-equal? (parse-string "x -> y -> x")
                (Program (list (Func (list (Local 'x 0))
                                     (Func (list (Local 'y 0))
                                           (Local 'x 0))))))
  (check-equal? (parse-string "(x -> x)(x)")
                (Program (list (Call (Func (list (Local 'x 0))
                                           (Local 'x 0))
                                     (list (Global #f 'x))))))
  (check-equal? (parse-string "x = (x -> x)(x)")
                (Program (list (Def (Global #f 'x) (Call (Func (list (Local 'x 0))
                                                               (Local 'x 0))
                                                         (list (Global #f 'x)))))))

  ; explicitly qualified identifiers
  (check-equal? (parse-string "m.x")
                (Program (list (Global 'm 'x))))
  (check-equal? (parse-string "x'0")
                ; x'0 is not in scope but we have to parse it faithfully
                (Program (list (Local 'x 0))))
  (check-equal? (parse-string "x -> x(m.x)")
                ; params don't shadow explicit globals
                (Program (list (Func (list (Local 'x 0))
                                     (Call (Local 'x 0)
                                           (list (Global 'm 'x)))))))
  (check-equal? (parse-string "x'0 -> x'1 -> x'0(x'1)")
                ; explicit numbers let you access parameters with the same name
                (Program (list (Func (list (Local 'x 0))
                                     (Func (list (Local 'x 1))
                                           (Call (Local 'x 0)
                                                 (list (Local 'x 1))))))))
  (check-equal? (parse-string "x'0 -> x -> x'0(x)")
                (Program (list (Func (list (Local 'x 0))
                                     (Func (list (Local 'x 1))
                                           (Call (Local 'x 0)
                                                 (list (Local 'x 1))))))))
  (check-equal? (parse-string "x -> x'1 -> x(x'1)")
                (Program (list (Func (list (Local 'x 0))
                                     (Func (list (Local 'x 1))
                                           (Call (Local 'x 0)
                                                 (list (Local 'x 1))))))))
  (check-equal? (parse-string "x -> x'0 -> x(x'0)")
                ; This case has to turn the x into x'1, because otherwise
                ; it would be shadowed by x'0.
                (Program (list (Func (list (Local 'x 1))
                                     (Func (list (Local 'x 0))
                                           (Call (Local 'x 1)
                                                 (list (Local 'x 0))))))))
  (check-equal? (parse-string "x -> x -> x'1 -> x(x'1)")
                ; The first x becomes x'0
                ; Next can't be x'1, so it's x'2
                ; Then the x in the body resolves to x'2
                (Program (list (Func (list (Local 'x 0))
                                     (Func (list (Local 'x 2))
                                           (Func (list (Local 'x 1))
                                                 (Call (Local 'x 2)
                                                       (list (Local 'x 1)))))))))

  ; imported globals
  ; "import" is in the Java sense: it lets you refer to identifiers by their unqualified names.
  ; This is also similar to Haskell import statements.
  ; - unqualified ids resolve to imported ids
  (check-equal? (parse-string/imports "x" (hash 'm (set 'x)))
                (Program (list (Global 'm 'x))))
  ; - conflicting imports-rev fails
  (check-exn exn:fail?
             (lambda () (parse-string/imports "x" (hash 'm0 (set 'x)
                                                        'm1 (set 'x))))
             "ambiguous")
  ; - but conflicting imports-rev only fail if the conflicted id is used
  (check-equal? (parse-string/imports "f(y)" (hash 'm0 (set 'x 'f)
                                                   'm1 (set 'x 'y)))
                ; m0 and m1 both export x, but this program doesn't use x.
                ; this program can still use identifiers from m0 and m1.
                (Program (list (Call (Global 'm0 'f)
                                     (list (Global 'm1 'y))))))
  ; - defs shadow imports-rev
  (check-equal? (parse-string/imports "x = 1 ; y = f(x)" (hash 'm (set 'x 'f)))
                (Program (list (Def (Global #f 'x) (Lit 1))
                               (Def (Global #f 'y) (Call (Global 'm 'f)
                                                         (list (Global #f 'x)))))))
  ; - params shadow imports-rev
  (check-equal? (parse-string/imports "x -> x" (hash 'm (set 'x)))
                (Program (list (Func (list (Local 'x 0))
                                     (Local 'x 0)))))

  ;;
  )
