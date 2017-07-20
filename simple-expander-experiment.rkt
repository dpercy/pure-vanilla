#lang racket


#|

Idea:
- simple expression expander
- custom set of primitive forms
- check each ID as a prim before desugaring

core:
x
(lambda (x ...) e)
(e e ...)
(if e e e)
(quote v)



|#

(define (atom? v)
  (and (not (symbol? v))
       (not (empty? v))
       (not (cons? v))))

(define default-interpretation
  ; primitives call expand on each subform
  (hash 'quote (match-lambda [(and form (list 'quote _)) form])
        'lambda (match-lambda [(list 'lambda (list (? symbol? args) ...) body)
                               `(lambda (,@args) ,(expand body))])
        'if (match-lambda [(list 'if test consq alt)
                           `(if ,(expand test) ,(expand consq) ,(expand alt))])
        'call (match-lambda [(list* 'call func args)
                             `(,(expand func) ,@(map expand args))])
        'var (match-lambda [(list 'var x) x])))
(define current-interpretation (make-parameter default-interpretation))

(define (expand form)
  (match form
    [(? atom?)   (apply-prim 'quote (list 'quote form))]
    [(? symbol?) (apply-prim 'var   (list 'var   form))]
    [(cons (? prim? head) _) (apply-prim head form)]
    [(cons 'and _) (expand (macro-and form))]
    [(cons 'let _) (expand (macro-let form))]
    [(cons _ _) (apply-prim 'call (cons 'call form))]
    ['() (error 'expand "empty parens not allowed")]))

(define (prim? v)
  (and (symbol? v)
       (hash-has-key? (current-interpretation) v)))

(define (apply-prim head form)
  (define h (hash-ref (current-interpretation)
                      head
                      (lambda ()
                        (error 'expand
                               "~v is not defined as a primitive. Valid primtives are: ~v"
                               head
                               (hash-keys (current-interpretation))))))
  (h form))


(define (macro-and form)
  (match form
    [(list 'and) #true]
    [(list 'and x y) `(if ,x ,y #false)]
    [(list* 'and x rest) `(and x (and ,@rest))]))

(define (macro-let form)
  (match form
    [(list 'let (list (list lhs rhs) ...) body)
     `((lambda (,@lhs) ,body) ,@rhs)]))


(module+ test
  (require rackunit)

  ; example elaborating surface scheme to core scheme
  (check-equal? (expand '(let ([x 1]) (g (and x (f 4)))))
                '((lambda (x) (g (if x (f '4) '#false)))
                  '1))

  ; elaborate to JS
  (check-equal? (parameterize ([current-interpretation
                                (hash
                                 'var (match-lambda
                                        [(list 'var x) (symbol->string x)])
                                 'quote (match-lambda
                                          [(list 'quote (? number? n)) (number->string n)])
                                 'lambda (match-lambda
                                           [(list 'lambda params body)
                                            (string-append "function("
                                                           (apply string-append
                                                                  (add-between (map expand params)
                                                                               ", "))
                                                           ") { return "
                                                           (expand body)
                                                           "; }")])
                                 'call (match-lambda
                                         [(list* 'call func args)
                                          (string-append "("
                                                         (expand func)
                                                         ")("
                                                         (apply string-append
                                                                (add-between (map expand args)
                                                                             ", "))
                                                         ")")])
                                 'and (match-lambda
                                        [(list* 'and args)
                                         (string-append "("
                                                        (apply string-append
                                                               (add-between (map expand args)
                                                                            " && "))
                                                        ")")]))])
                  (expand '(let ([x 1]) (g (and x (f 4))))))
                "(function(x) { return (g)((x && (f)(4))); })(1)")

  ; TODO What enforces prims' shape when they're customized?
  ;      A stupid custom quote could accept zero or two args.
  ;      This isn't good.
  ;      Customizations should only change the expansion,
  ;        not the validation


  ;;
  )
