#lang racket

(provide (all-defined-out))

(require racket/splicing
         racket/syntax
         (for-syntax racket/syntax))


(struct Tagged (tag values) #:transparent)


(define/contract (make-symbol mod name) (-> string? string? symbol?)
  (format-symbol "~a.~a" mod name))


(define/contract (tag t args) (-> symbol? list? Tagged?)
  (Tagged t args))


(define/contract (untag t v) (-> symbol? any/c (or/c #false list?))
  (match v
    [(Tagged (== t) args) args]
    [(Tagged _ _) #false]))


(define-syntax (tag-struct stx)
  (syntax-case stx ()
    [(_ mod-string (name args ...)) (and (string? (syntax-e #'mod-string))
                                         (identifier? #'name)
                                         (andmap identifier? (syntax->list #'(args ...))))
     (with-syntax ([predicate (format-id #'name "~a?" #'name)]
                   [(accessor ...) (for/list ([arg (syntax->list #'(args ...))])
                                     (format-id #'name "~a-~a" #'name arg))]
                   [(index ...) (build-list (length (syntax->list #'(args ...)))
                                            values)]
                   [name-string (symbol->string (syntax-e #'name))])
       #'(begin
           (splicing-let ([t (make-symbol mod-string name-string)])
             (define-match-expander name
               ; pat
               (syntax-rules ()
                 [(_ args ...)  (app (curry untag t) (list args ...))])
               ; expr
               (make-set!-transformer
                (lambda (stx)
                  (if (identifier? stx)
                      #'(lambda (args ...) (name args ...))
                      ((syntax-rules ()
                         [(_ args ...)  (tag t (list args ...))])
                       stx)))))
             (define (predicate v)
               (and (Tagged? v) (equal? (Tagged-tag v) t)))
             (define/contract (accessor v)
               (-> predicate any/c)
               (list-ref (untag t v) index)) ...
             ;;
             )))]))
