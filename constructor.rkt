#lang racket

(provide (all-defined-out))

(require racket/splicing
         (for-syntax racket/syntax))


(struct Global (mod name) #:transparent)
(struct Tagged (tag values) #:transparent)

(define/contract (tag t args) (-> Global? list? (or/c Global? Tagged?))
  (if (equal? t (Global "Syntax" "Global"))
      (apply Global args)
      (Tagged t args)))

(define/contract (untag t v) (-> Global? (or/c Global? Tagged?) (or/c #false list?))
  (cond
    [(and (Global? v)
          (equal? t (Global "Syntax" "Global")))  (list (Global-mod v)
                                                        (Global-name v))]
    [(and (Tagged? v)
          (equal? t (Tagged-tag v)))  (Tagged-values v)]
    [else #false]))


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
           (splicing-let ([t (Global mod-string name-string)])
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
