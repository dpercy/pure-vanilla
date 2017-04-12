#lang racket


#|

Macro AST experiment!

I want a data structure that looks like s-expressions,
but which you can analyze as a proper tree.

|#



; an unresolved name is just a symbol
; an unresolved form is just a list

; a proper AST
(struct Lit (v) #:transparent)
(struct Local (name num) #:transparent)
(struct Global (name) #:transparent)
(struct Func (params body) #:transparent)
(struct If (test consq alt) #:transparent)
(struct Call (func args) #:transparent)
(struct Macro (name args result) #:transparent)


; intermediate forms while parsing
; (kind of like logic variables)
(struct IM (sexp vb) #:transparent)


(define (and-macro a b)
  `(if ,a ,b #false))

(define (let-macro binds body)
  (match binds
    [(IM (list (IM (list lhs rhs) _) ...) _)
     `(call (lambda ,lhs ,body)
            ,@rhs)]))


(define (p sexp)
  (set! counter 0)
  (extract (parse (instrument sexp))))

(define (instrument sexp)
  (define (wrap sexp)
    (IM sexp (box #false)))
  (match sexp
    [(? IM?) sexp]
    [(? list?) (wrap (map instrument sexp))]
    [_ (wrap sexp)]))
(define (extract intm)
  (define (unwrap intm)
    (match intm
      [(IM _ (box (? (not/c false?) v))) (unwrap v)]
      [(IM v _) (unwrap v)]
      [v v]))
  (match (unwrap intm)
    [(? list? intm) (map extract intm)]
    [(If t c a) (If (extract t)
                    (extract c)
                    (extract a))]
    [(Func params body) (Func (extract params)
                              (extract body))]
    [(Macro name args result) (Macro (extract name)
                                     (extract args)
                                     (extract result))]
    [v v]))
(define (parse intm)
  (let parse ([intm intm]
              [env (hash)])
    (define (recur intm) (parse intm env))
    (let ([v (match (IM-sexp intm)
               [(? number? n) (Lit n)]
               [(? boolean? b) (Lit b)]
               [(? symbol? s) (hash-ref env s
                                        (lambda () (Global s)))]
               [(list (app IM-sexp 'if) test consq alt) (If (recur test) (recur consq) (recur alt))]
               [(list (app IM-sexp 'lambda) (app IM-sexp params) body)
                (let* ([locals (map parse-param params)]
                       [names (map IM-sexp params)]
                       [env (for/fold ([env env]) ([n names] [l locals])
                              (hash-set env n l))])
                  (Func locals (parse body env)))]
               [(list* (app IM-sexp 'call) func args) (Call (recur func) (map recur args))]
               ; macros!
               [(list* (app IM-sexp 'and) args)
                (Macro 'and args
                       (recur (instrument (apply and-macro args))))]
               [(list* (app IM-sexp 'let) args)
                (Macro 'let args
                       (recur (instrument (apply let-macro args))))])])
      (set-box! (IM-vb intm) v)
      v)))
(define counter 0)
(define (get-counter!)
  (define c counter)
  (set! counter (+ 1 counter))
  c)
(define (parse-param intm)
  (match intm
    [(IM (? symbol? s) b)
     (let* ([n (get-counter!)]
            [v (Local s n)])
       (set-box! b v)
       v)]))


(module+ test
  (require rackunit)

  (check-equal? (p '2) (Lit 2))
  (check-equal? (p '#false) (Lit #false))
  (check-equal? (p 'x) (Global 'x))
  (check-equal? (p '(if 1 2 3)) (If (Lit 1) (Lit 2) (Lit 3)))
  (check-equal? (p '(lambda () x)) (Func '() (Global 'x)))
  (check-equal? (p '(lambda (x) x)) (Func (list (Local 'x 0)) (Local 'x 0)))
  (check-equal? (p '(and x 2))
                (Macro 'and
                       (list (Global 'x)
                             (Lit 2))
                       (If (Global 'x) (Lit 2) (Lit #false))))
  (check-equal? (p '(call 1 2 3))
                (Call (Lit 1) (list (Lit 2) (Lit 3))))
  (check-equal? (p '(let ([a a] [b 2]) a))
                (Macro 'let
                       (list (list (list (Local 'a 0) (Global 'a))
                                   (list (Local 'b 1) (Lit 2)))
                             (Local 'a 0))
                       (Call (Func (list (Local 'a 0)
                                         (Local 'b 1))
                                   (Local 'a 0))
                             (list (Global 'a)
                                   (Lit 2))))))
