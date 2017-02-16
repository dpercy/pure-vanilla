#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)


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
(struct Local (name number) #:transparent)
(struct Global (mod name) #:transparent)

(struct Func (params body) #:transparent)
(struct Call (func args) #:transparent)

(define parse
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
    (Iden [(UnqualIden) (Global #f $1)] ; TODO pass in the default namespace and current scope
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

(define (parse-string str)
  (let* ([port (open-input-string str)]
         [lex! (lambda () (lex port))])
    (parse lex!)))
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
  (check-equal? (parse-string "(x) -> 3") (Program (list (Func (list (Global #f 'x))
                                                               (Lit 3)))))

  ; function arity
  (check-equal? (parse-string "(x,) -> 3") (Program (list (Func (list (Global #f 'x))
                                                                (Lit 3)))))
  (check-equal? (parse-string "(x,y) -> 3") (Program (list (Func (list (Global #f 'x)
                                                                       (Global #f 'y))
                                                                 (Lit 3)))))
  (check-equal? (parse-string "(x,y,) -> 3") (Program (list (Func (list (Global #f 'x)
                                                                        (Global #f 'y))
                                                                  (Lit 3)))))

  ; expr nesting inside parens
  (check-equal? (parse-string "((x))") (Program (list (Global #f 'x))))
  (check-equal? (parse-string "(((x)))") (Program (list (Global #f 'x))))
  (check-equal? (parse-string "(x -> 1)") (Program (list (Func (list (Global #f 'x))
                                                               (Lit 1))))))
