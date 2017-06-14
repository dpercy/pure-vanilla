#lang racket

(provide construct
         destruct
         make-symbol
         typetag
         tagof

         Tagged?
         Typetag?

         tag-struct
         tag-structs)

(require racket/splicing
         racket/syntax
         (for-syntax racket/syntax))


(struct Tagged (tag values) #:transparent)
(struct Typetag (tag) #:transparent)


(define/contract (make-symbol mod name) (-> string? string? symbol?)
  (format-symbol "~a.~a" mod name))


(define/contract (construct t args) (-> symbol? list? Tagged?)
  (Tagged t args))


(define/contract (destruct t v) (-> symbol? any/c (or/c #false list?))
  (match v
    [(Tagged (== t) args) args]
    [(Tagged _ _) #false]
    [_ #false]))


(define/contract (typetag t) (-> symbol? Typetag?)
  (Typetag t))


(define/contract (tagof v) (-> Tagged? Typetag?)
  (match v
    [(Tagged t _) (typetag t)]))


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
                 [(_ args ...)  (app (curry destruct t) (list args ...))])
               ; expr
               (make-set!-transformer
                (lambda (stx)
                  (if (identifier? stx)
                      #'(lambda (args ...) (name args ...))
                      ((syntax-rules ()
                         [(_ args ...)  (construct t (list args ...))])
                       stx)))))
             (define (predicate v)
               (and (Tagged? v) (equal? (tagof v) (typetag t))))
             (define/contract (accessor v)
               (-> predicate any/c)
               (list-ref (destruct t v) index)) ...
             ;;
             )))]))
(module+ test
  (require rackunit)

  (tag-struct "Foo" (Bar x y))

  (check-equal? (format "~v" (Bar 1 2))
                "(Tagged 'Foo.Bar '(1 2))")
  (check-pred Bar? (Bar 1 2))
  (check-equal? (tagof (Bar 1 2)) (typetag 'Foo.Bar))

  (match (Bar 1 2)
    [(Bar x y) (begin
                 (check-equal? x 1)
                 (check-equal? y 2))]))

(define-syntax-rule (tag-structs name forms ...)
  (begin
    (tag-struct name forms) ...))
