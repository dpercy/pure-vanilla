#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)
(require parser-tools/cfg-parser)
(require syntax/srcloc)
(require "./util.rkt")

(provide (all-from-out 'ast)
         parse-string
         parse-string/imports
         parse-port/imports
         )


(define-tokens nonempty-tokens (Literal Identifier Operator))
(define-empty-tokens empty-tokens (EOF Equals Newline OpenParen CloseParen OpenBracket CloseBracket Arrow Comma If Then Else Let In))

(define-lex-abbrev digit (char-range #\0 #\9))
(define-lex-abbrev letter (union (char-range #\a #\z)
                                 (char-range #\A #\Z)))

(define-lex-abbrev iden (concatenation letter
                                       (repetition 0 +inf.0 (union letter digit))))
(define-lex-abbrev op (repetition 1 +inf.0 (char-set "~!@$%^&*-+=:<>/?|\\")))

(define-lex-abbrev nat (repetition 1 +inf.0 digit))
(define-lex-abbrev comment (concatenation #\#
                                          (repetition 0 +inf.0 (char-complement #\newline))))
(define-lex-abbrev maybe-comment (repetition 0 1 comment))
(define lex
  (lexer-src-pos
   ; TODO implement comments as an actual token?
   ; whitespace with no newlines is ignored
   [(repetition 1 +inf.0
                (intersection whitespace
                              (char-complement #\newline)))  (return-without-pos (lex input-port))]
   [comment (return-without-pos (lex input-port))]
   ; a newline plus any mixture of whitespace and newlines is a Newline token
   [(concatenation #\newline
                   (repetition 0 +inf.0
                               (concatenation maybe-comment
                                              whitespace)))  (token-Newline)]
   [";" (token-Newline)]


   [nat  (token-Literal (string->number lexeme))]
   [(concatenation #\"
                   (repetition 0 +inf.0 (char-complement (union #\\
                                                                #\")))
                   #\")
    ; TODO escapes
    (token-Literal (read (open-input-string lexeme)))]

   ["[" (token-OpenBracket)]
   ["]" (token-CloseBracket)]
   ["(" (token-OpenParen)]
   [")" (token-CloseParen)]
   ["," (token-Comma)]


   ; identifiers
   [(concatenation iden #\. nat) (match (string-split lexeme ".")
                                   [(list name num) (token-Identifier (Local #f
                                                                             (string->symbol name)
                                                                             (string->number num)))])]
   [(concatenation iden #\. iden)  (match (string-split lexeme ".")
                                     [(list qual name)
                                      (token-Identifier (Global #f
                                                                (string->symbol qual)
                                                                (string->symbol name)))])]
   [iden  (match lexeme
            ["if"   (token-If)]
            ["then" (token-Then)]
            ["else" (token-Else)]
            ["let"  (token-Let)]
            ["in"   (token-In)]
            [_
             (token-Identifier (Unresolved #f (string->symbol lexeme)))])]

   ; operators
   [(concatenation op #\. nat) (match (string-split lexeme ".")
                                 [(list name num) (token-Operator (Local #f
                                                                         (string->symbol name)
                                                                         (string->number num)))])]
   [(concatenation iden #\. op)  (match (string-split lexeme ".")
                                   [(list qual name)
                                    (token-Operator (Global #f
                                                            (string->symbol qual)
                                                            (string->symbol name)))])]
   [op  (match lexeme
          ["->" (token-Arrow)]
          ["=" (token-Equals)]
          [_
           (token-Operator (Unresolved #f (string->symbol lexeme)))])]


   [(eof)  (token-EOF)]))

(define (lex-string str)
  (let ([port (open-input-string str)])
    (port-count-lines! port)
    (sequence->list (in-producer (lambda () (curry lex port))
                                 (lambda (tok)
                                   (or (equal? tok (token-EOF))
                                       (equal? (position-token-token tok) (token-EOF))))))))


(module ast racket
  (provide (all-defined-out))

  (struct Syntax (loc) #:transparent)

  (struct Program Syntax (statements) #:transparent)
  (struct Def Syntax (var expr) #:transparent)

  (struct Lit Syntax (value) #:transparent)
  ; `number` is not a De-Bruijn index or up-reference:
  ; it's an extension of the variable's name.
  ; Two locals are equal iff their `name` and `number` are equal.
  (struct Local Syntax (name number) #:transparent)
  (struct Global Syntax (mod name) #:transparent)
  ; An Unresolved expression doesn't properly belong in an Expr.
  ; It should never be observable by the user program.
  (struct Unresolved Syntax (name) #:transparent)

  (struct Func Syntax (params body) #:transparent)
  (struct Call Syntax (func args) #:transparent)
  (struct If Syntax (test consq alt) #:transparent)

  )
(require 'ast)

(define-syntax (pos stx)
  (with-syntax ([(source-name start end)
                 (datum->syntax stx '(source-name $1-start-pos $n-end-pos))])
    #'(make-loc source-name start end)))
(define (make-loc source-name start end)
  (define (convert pos)
    (match pos
      [(position offset line col)
       (srcloc source-name
               line
               col
               offset
               0)]))
  (build-source-location-list (convert start) (convert end)))
(define (pos-id pos id)
  (match id
    [(Unresolved _ name) (Unresolved pos name)]
    [(Local _ name num) (Local pos name num)]
    [(Global _ mod name) (Global pos mod name)]))

(define (pre-parse source-name)
  (cfg-parser
   (src-pos)
   (tokens nonempty-tokens empty-tokens)
   (start Prog)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value $1-start-pos $n-end-pos)
            (raise-syntax-error tok-name
                                "Unexpected token"
                                (datum->syntax #f tok-value (pos)))
            #;
            (error 'parse "Unexpected token ~v ~v" tok-name tok-value)))
   (grammar
    (N [(Newline) (void)]
       [() (void)])
    (Prog [(Statements) (Program (pos) $1)]
          [(Newline Statements) (Program (pos) $2)]
          )
    (Statements [() (list)]
                [(Statement) (list $1)]
                [(Statement Newline Statements) (cons $1 $3)])
    (Statement [(Definition) $1] [(Expr) $1])
    (Definition
      [(Iden Equals Expr) (Def (pos) $1 $3)]
      [(OpenParen Op CloseParen Equals Expr) (Def (pos) $2 $5)]
      )

    (Iden [(Identifier) (pos-id (pos) $1)])
    (Op [(Operator) (pos-id (pos) $1)])
    (Expr [(If Expr N Then Expr N Else Expr) (If (pos) $2 $5 $8)]

          [(Iden Arrow N Expr) (Func (pos) (list $1) $4)]
          [(OpenParen Params CloseParen Arrow N Expr) (match (ormap (not/c (or/c Unresolved?
                                                                                 Local?))
                                                                    $2)
                                                        [#false (Func (pos) $2 $6)]
                                                        [bad (error "bad parameter: ~v" bad)])]

          [(Let Iden Equals Expr In N Expr) (Call (pos)
                                                  (Func #f (list $2)
                                                        $7)
                                                  (list $4))]

          [(Arith) $1])
    (Arith [(Term Op Term) (Call (pos) $2 (list $1 $3))]
           [(Op Term) (Call (pos) $1 (list $2))]
           [(Term) $1])
    (Term [(Term OpenParen Args CloseParen) (Call (pos) $1 $3)]
          [(OpenBracket Args CloseBracket) (Call (pos) (Global #f 'Base 'list) $2)]

          [(Iden) $1]
          [(Literal) (Lit (pos) $1)]
          [(Op) $1]

          ; paren nest
          [(OpenParen Expr CloseParen) $2])
    (Params [() (list)]
            [(Param) (list $1)]
            [(Param Comma Args) (cons $1 $3)])
    (Param [(Iden) $1]
           [(Op) $1])
    (Args [(N) (list)] ; empty case, or base case with trailing comma
          [(N Expr N) (list $2)] ; base case with no trailing comma
          [(N Expr Comma Args) (cons $2 $4)]
          ; op cases
          [(N Op N) (list $1)]
          [(N Op Comma Args) (cons $2 $4)]
          ))))

(define (parse/imports lex! imports source-name)
  (fix-scope-program ((pre-parse source-name) lex!) (invert-hash-of-sets imports)))

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
    [(Program loc statements)
     ; local-env : name -> number, the max of in-scope locals
     (define local-env (hash))
     (define imports-rev*
       (for/fold ([imports-rev imports-rev]) ([stmt statements])
         (match stmt
           [(Def _ (Unresolved _ name) _) (hash-remove imports-rev name)]
           [_ imports-rev])))
     (Program loc
              (for/list ([stmt statements])
                (fix-scope-statement stmt imports-rev* local-env)))]))
(define (fix-scope-statement stmt imports-rev local-env)
  (match stmt
    [(Def loc var expr) (Def loc
                          (fix-scope-expr var imports-rev local-env)
                          (fix-scope-expr expr imports-rev local-env))]
    [expr (fix-scope-expr expr imports-rev local-env)]))
(define (fix-scope-expr expr imports-rev local-env)
  (define (recur expr) (fix-scope-expr expr imports-rev local-env))
  (match expr
    [(Local _ name number)  expr]
    [(Global _ mod name)  expr]


    [(Lit _ value)  expr]
    [(Unresolved loc name)  (match (hash-ref local-env name #f)
                              [(? number? n)  (Local loc name n)]
                              [#false

                               (match (hash-ref imports-rev name #f)
                                 ; TODO should unbound unqual ID be an error?
                                 ; IDs should refer to definitions!
                                 [#false (Global loc #f name)]
                                 [(app unwrap-unary-set (list m)) (Global loc m name)]
                                 [choices (error 'fix-scope
                                                 "Ambiguous reference ~a: could refer to ~a"
                                                 name
                                                 (string-join (for/list ([m choices])
                                                                (format "~s.~s" m name))
                                                              " or "))])])]

    [(Func loc params body)
     (let ([local-env* (for/fold ([local-env local-env]) ([p params])
                         (match p
                           [(Local _ _ _) local-env]
                           [(Unresolved _ name)
                            ; What's the new identifier number?
                            ; It should be 1 higher than the highest already in scope,
                            ; or 0 if none are in scope.
                            (define num (+ 1 (hash-ref local-env name -1)))
                            ; But also, it must not be used as a parameter anywhere in body.
                            (while (set-member? (explicit-locals body)
                                                (Local #f name num))
                              (set! num (+ num 1)))
                            (hash-set local-env name num)]))])
       (Func loc
             (for/list ([p params])
               (fix-scope-expr p imports-rev local-env*))
             (fix-scope-expr body imports-rev local-env*)))]
    [(Call loc func args)  (Call loc
                                 (recur func)
                                 (map recur args))]
    [(If loc test consq alt) (If loc
                                 (recur test)
                                 (recur consq)
                                 (recur alt))]))

(define explicit-locals ; expr -> set( Local )
  (memoize
   (make-weak-hasheq)
   (lambda (expr)
     (match expr
       [(Local _ name number)  (set (Local #f name number))]
       [(Global _ mod name)  (set)]


       [(Lit _ value)  (set)]
       [(Unresolved _ name)  (set)]

       [(Func _ params body) (apply set-union
                                    (map explicit-locals (cons body params)))]
       [(Call _ func args)  (apply set-union
                                   (map explicit-locals (cons func args)))]
       [(If _ test consq alt)  (apply set-union
                                      (map explicit-locals (list test consq alt)))]))))

(define (parse-string/imports str imports)
  (let* ([port (open-input-string str)]
         [lex! (lambda () (lex port))])
    (port-count-lines! port)
    (parse/imports lex! imports 'input-string)))
(define (parse-string str)
  (parse-string/imports str (hash)))
(define (parse-port/imports port imports source-name)
  (let* ([lex! (lambda () (lex port))])
    (parse/imports lex! imports source-name)))
(module+ test
  (require rackunit)

  ; whitespace
  (check-match (parse-string "1") (Program _ (list (Lit _ 1))))
  (check-match (parse-string "1\n") (Program _ (list (Lit _ 1))))
  (check-match (parse-string "1\n\n\n") (Program _ (list (Lit _ 1))))
  (check-match (parse-string "\n1") (Program _ (list (Lit _ 1))))
  (check-match (parse-string "\n\n\n1") (Program _ (list (Lit _ 1))))

  (check-match (parse-string "f(x, 1)")
               (Program _ (list (Call _ (Global _ #f 'f) (list (Global _ #f 'x)
                                                               (Lit _ 1))))))
  (check-match (parse-string "f = () -> 4")
               (Program _ (list (Def _ (Global _ #f 'f) (Func _ '() (Lit _ 4))))))

  ; application binds tighter than abstraction
  (check-match (parse-string "() -> 1")
               (Program _ (list (Func _ '() (Lit _ 1)))))

  ; parenthesized expr vs function
  (check-match (parse-string "(x)") (Program _ (list (Global _ #f 'x))))
  (check-match (parse-string "(x) -> 3") (Program _ (list (Func _ (list (Local _ 'x 0))
                                                                (Lit _ 3)))))

  ; function arity
  (check-match (parse-string "(x,) -> 3") (Program _ (list (Func _ (list (Local _ 'x 0))
                                                                 (Lit _ 3)))))
  (check-match (parse-string "(x,y) -> 3") (Program _ (list (Func _ (list (Local _ 'x 0)
                                                                          (Local _ 'y 0))
                                                                  (Lit _ 3)))))
  (check-match (parse-string "(x,y,) -> 3") (Program _ (list (Func _ (list (Local _ 'x 0)
                                                                           (Local _ 'y 0))
                                                                   (Lit _ 3)))))

  ; expr nesting inside parens
  (check-match (parse-string "((x))") (Program _ (list (Global _ #f 'x))))
  (check-match (parse-string "(((x)))") (Program _ (list (Global _ #f 'x))))
  (check-match (parse-string "(x -> 1)") (Program _ (list (Func _ (list (Local _ 'x 0))
                                                                (Lit _ 1)))))

  ; scope and shadowing
  (check-match (parse-string "x -> x")
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Local _ 'x 0)))))
  (check-match (parse-string "x -> x -> x")
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Func _ (list (Local _ 'x 1))
                                            (Local _ 'x 1))))))
  (check-match (parse-string "x -> y -> x")
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Func _ (list (Local _ 'y 0))
                                            (Local _ 'x 0))))))
  (check-match (parse-string "(x -> x)(x)")
               (Program _ (list (Call _ (Func _ (list (Local _ 'x 0))
                                              (Local _ 'x 0))
                                      (list (Global _ #f 'x))))))
  (check-match (parse-string "x = (x -> x)(x)")
               (Program _ (list (Def _ (Global _ #f 'x) (Call _ (Func _ (list (Local _ 'x 0))
                                                                      (Local _ 'x 0))
                                                              (list (Global _ #f 'x)))))))

  ; explicitly qualified identifiers
  (check-match (parse-string "m.x")
               (Program _ (list (Global _ 'm 'x))))
  (check-match (parse-string "x.0")
               ; x.0 is not in scope but we have to parse it faithfully
               (Program _ (list (Local _ 'x 0))))
  (check-match (parse-string "x -> x(m.x)")
               ; params don't shadow explicit globals
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Call _ (Local _ 'x 0)
                                            (list (Global _ 'm 'x)))))))
  (check-match (parse-string "x.0 -> x.1 -> x.0(x.1)")
               ; explicit numbers let you access parameters with the same name
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Func _ (list (Local _ 'x 1))
                                            (Call _ (Local _ 'x 0)
                                                  (list (Local _ 'x 1))))))))
  (check-match (parse-string "x.0 -> x -> x.0(x)")
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Func _ (list (Local _ 'x 1))
                                            (Call _ (Local _ 'x 0)
                                                  (list (Local _ 'x 1))))))))
  (check-match (parse-string "x -> x.1 -> x(x.1)")
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Func _ (list (Local _ 'x 1))
                                            (Call _ (Local _ 'x 0)
                                                  (list (Local _ 'x 1))))))))
  (check-match (parse-string "x -> x.0 -> x(x.0)")
               ; This case has to turn the x into x.1, because otherwise
               ; it would be shadowed by x.0.
               (Program _ (list (Func _ (list (Local _ 'x 1))
                                      (Func _ (list (Local _ 'x 0))
                                            (Call _ (Local _ 'x 1)
                                                  (list (Local _ 'x 0))))))))
  (check-match (parse-string "x -> x -> x.1 -> x(x.1)")
               ; The first x becomes x.0
               ; Next can't be x.1, so it's x.2
               ; Then the x in the body resolves to x.2
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Func _ (list (Local _ 'x 2))
                                            (Func _ (list (Local _ 'x 1))
                                                  (Call _ (Local _ 'x 2)
                                                        (list (Local _ 'x 1)))))))))

  ; imported globals
  ; "import" is in the Java sense: it lets you refer to identifiers by their unqualified names.
  ; This is also similar to Haskell import statements.
  ; - unqualified ids resolve to imported ids
  (check-match (parse-string/imports "x" (hash 'm (set 'x)))
               (Program _ (list (Global _ 'm 'x))))
  ; - conflicting imports-rev fails
  (check-exn exn:fail?
             (lambda () (parse-string/imports "x" (hash 'm0 (set 'x)
                                                        'm1 (set 'x))))
             "ambiguous")
  ; - but conflicting imports-rev only fail if the conflicted id is used
  (check-match (parse-string/imports "f(y)" (hash 'm0 (set 'x 'f)
                                                  'm1 (set 'x 'y)))
               ; m0 and m1 both export x, but this program doesn't use x.
               ; this program can still use identifiers from m0 and m1.
               (Program _ (list (Call _ (Global _ 'm0 'f)
                                      (list (Global _ 'm1 'y))))))
  ; - defs shadow imports-rev
  (check-match (parse-string/imports "x = 1 ; y = f(x)" (hash 'm (set 'x 'f)))
               (Program _ (list (Def _ (Global _ #f 'x) (Lit _ 1))
                                (Def _ (Global _ #f 'y) (Call _ (Global _ 'm 'f)
                                                              (list (Global _ #f 'x)))))))
  ; - params shadow imports-rev
  (check-match (parse-string/imports "x -> x" (hash 'm (set 'x)))
               (Program _ (list (Func _ (list (Local _ 'x 0))
                                      (Local _ 'x 0)))))


  ; operator syntax
  ; - op can be a defined name, parameter, or expression
  (check-match (parse-string "(+) = 1") (Program _ (list (Def _ (Global _ #f '+) (Lit _ 1)))))
  (check-match (parse-string "(+) -> 1") (Program _ (list (Func _ (list (Local _ '+ 0)) (Lit _ 1)))))
  (check-match (parse-string "(+, y) -> 1") (Program _ (list (Func _ (list (Local _ '+ 0)
                                                                           (Local _ 'y 0)) (Lit _ 1)))))
  (check-match (parse-string "(+)") (Program _ (list (Global _ #f '+))))
  (check-match (parse-string "(math.+)") (Program _ (list (Global _ 'math '+))))
  (check-match (parse-string "(+.3)") (Program _ (list (Local _ '+ 3))))
  ; - simple infix
  (check-match (parse-string "x + y")
               (Program _ (list (Call _ (Global _ #f '+)
                                      (list (Global _ #f 'x)
                                            (Global _ #f 'y))))))
  (check-match (parse-string "x +.123 y")
               (Program _ (list (Call _ (Local _ '+ 123)
                                      (list (Global _ #f 'x)
                                            (Global _ #f 'y))))))
  (check-match (parse-string "x math.+ y")
               (Program _ (list (Call _ (Global _ 'math '+)
                                      (list (Global _ #f 'x)
                                            (Global _ #f 'y))))))
  ; - op is nonassoc
  (check-exn exn:fail?
             (lambda () (parse-string "x + y * z")))
  (check-match (parse-string "x + (y * z)")
               (Program _ (list (Call _ (Global _ #f '+)
                                      (list (Global _ #f 'x)
                                            (Call _ (Global _ #f '*)
                                                  (list (Global _ #f 'y)
                                                        (Global _ #f 'z))))))))
  ; - op binds tighter than lambda
  (check-match (parse-string "() -> x + y")
               (Program _ (list (Func _ '()
                                      (Call _ (Global _ #f '+)
                                            (list (Global _ #f 'x)
                                                  (Global _ #f 'y)))))))
  ; - op binds looser than call
  (check-match (parse-string "x + y()")
               (Program _ (list (Call _ (Global _ #f '+)
                                      (list (Global _ #f 'x)
                                            (Call _ (Global _ #f 'y) '()))))))

  ; - prefix
  (check-match (parse-string "- x")
               (Program _ (list (Call _ (Global _ #f '-) (list (Global _ #f 'x))))))
  ; - prefix nonassoc with infix
  (check-exn exn:fail?
             (lambda () (parse-string "- x + y")))





  ;; bigger tests
  (check-match (parse-string "mkpair = (left, right) -> selector -> selector(left, right)")
               (Program _
                        (list
                         (Def _ (Global _ #f 'mkpair)
                           (Func _ (list (Local _ 'left 0)
                                         (Local _ 'right 0))
                                 (Func _ (list (Local _ 'selector 0))
                                       (Call _ (Local _ 'selector 0)
                                             (list (Local _ 'left 0)
                                                   (Local _ 'right 0)))))))))

  ;;
  )
