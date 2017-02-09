#lang racket

(require parser-tools/lex)
(require parser-tools/yacc)


(define-tokens tokens (Iden Num))
(define-empty-tokens empty-tokens (EOF Equals Newline))

(define lex
  (lexer
   ; TODO implement comments as an actual token?
   ; whitespace with no newlines is ignored
   [(repetition 0 +inf.0 (intersection whitespace
                                       (char-complement #\newline))) (lex input-port)]
   ; a newline plus any mixture of whitespace and newlines is a Newline token
   [(concatenation #\newline (repetition 0 +inf.0 whitespace)) (token-Newline)]
   [(repetition 1 +inf.0 (char-range #\0 #\9))  (token-Num (string->number lexeme))]
   [(eof) (token-EOF)]))

#|
(struct Program (defs) #:transparent)
(struct Def (var expr) #:transparent)

(struct Lit (value) #:transparent)
(struct Local (name number) #:transparent)
(struct Global (mod name) #:transparent)


(define parse
  (parser
   (tokens TODO)
   (start Program)
   (end eof?)

   ))

|#
