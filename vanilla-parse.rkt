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



(struct Program (defs) #:transparent)
(struct Def (var expr) #:transparent)

(struct Lit (value) #:transparent)
(struct Local (name number) #:transparent)
(struct Global (mod name) #:transparent)

(struct Func (params body) #:transparent)

(define parse
  (parser
   (tokens tokens empty-tokens)
   (start Program)
   (end EOF)
   (error (lambda (tok-ok? tok-name tok-value)
            (error 'parse "Unexpected token ~v ~v" tok-name tok-value)))
   (grammar
    (Program [() (list)]
             [(DefOrExpr) (list $1)]
             [(DefOrExpr Newline Program) (cons $1 $3)])
    (DefOrExpr [(Def) $1] [(Expr) $1])
    (Def [(Iden Equals Expr) (Def $1 $3)])
    (Iden [(UnqualIden) (Global #f $1)] ; TODO pass in the default namespace and current scope
          [(IdenNum) (match $1 [(list name num) (Local name num)])]
          [(QualIden) (match $1 [(list mod name) (Global mod name)])])
    (Expr [(Num) (Lit $1)]
          [(Iden) $1]
          [(OpenParen Params CloseParen Arrow Expr) (Func $2 $5)]
          )
    (Params [() (list)] ; empty case, or base case with trailing comma
            [(Iden) (list $1)] ; base case with no trailing comma
            [(Iden Comma Params) (cons $1 $3)]))))

(define (parse-string str)
  (let* ([port (open-input-string str)]
         [lex! (lambda () (lex port))])
    (parse lex!)))
