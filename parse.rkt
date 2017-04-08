#lang racket

(provide parse)

(require "ast.rkt")


#|

Limitations of this parser:
- assumes dotted identifiers are globals (doesn't support x.0 notation for locals)

|#


(define (parse form)
  (let parse ([form form]
              [scope (empty-scope)])
    (define (r form) (parse form scope))
    (match form
      ; module
      [(list 'module mn body ...)
       (let ([scope (scope-set-modname scope mn)])
         (Module mn (for/list ([b body])
                      (parse b scope))))]
      ; statement
      [`(using ,mn)  (Using mn)]
      [`(= ,lhs ,rhs) (Def (parse-var lhs scope) (r rhs))]
      ; expression
      [(? symbol?) (parse-var form scope)]
      [(? self-quoting?)  (Lit form)]
      [`(quote ,v)  (Lit v)]
      [`(quote ,@_) (error (format "bad quote form: ~v" form))]
      [`(syntax ,v) (Quote (r v))]
      [`(syntax ,@_) (error (format "bad syntax form: ~v" form))]
      [`(if ,t ,c ,a) (If (r t) (r c) (r a))]
      [`(if ,@_) (error (format "bad if form: ~v" form))]
      [`(lambda ,vars ,body) (let* ([params (map (make-local scope) vars)]
                                    [scope (foldr scope-add scope params)])
                               (Func params
                                     (parse body scope)))]
      [`(lambda ,@_) (error (format "bad lambda form: ~v" form))]

      ; macros / syntax sugar
      [`(let ([,lhs ,rhs] ...) ,body) (r `((lambda ,lhs ,body) ,@rhs))]
      [`(let ,@_) (error (format "bad let form: ~v" form))]
      [`(let* ([,lhs ,rhs] ...) ,body) (r (foldr (lambda (l r b) `(let ([,l ,r]) ,b))
                                                 body lhs rhs))]
      [`(let* ,@_) (error (format "bad let* form: ~v" form))]
      [`(cond [,lhs ,rhs] ... [else ,final]) (r (foldr (lambda (l r c) `(if ,l ,r ,c))
                                                       final lhs rhs))]
      [`(cond ,cases ...) (r `(cond ,@cases [else (error "no case")]))]
      [`(cond ,@_) (error (format "bad cond form: ~v" form))]
      [`(begin ,forms ... ,last) (r (foldr (lambda (stmt expr)
                                             `(let ([_ ,stmt]) ,expr))
                                           last
                                           forms))]
      [`(begin ,@_) (error (format "bad begin form: ~v" form))]
      [(cons head tail) (Call (r head) (map r tail))])))


(define (parse-var form scope)
  (match (string-split (symbol->string form) ".")
    [(list _) (match (hash-ref (Scope-innermost scope) form #f)
                [#f (Global (Scope-modname scope) form)]
                [n (Local form n)])]
    [(list a b) (Global (string->symbol a)
                        (string->symbol b))]))

(define (self-quoting? v)
  (and (not (pair? v))
       (not (symbol? v))))


(define ((make-local scope) name)
  (Local name (+ 1 (hash-ref (Scope-innermost scope) name -1))))

(define (scope-contains? scope local)
  (match-define (Local name num) local)
  (<= num (hash-ref (Scope-innermost scope) name -1)))

(define (scope-add local scope)
  (match-define (Scope modname innermost) scope)
  (match-define (Local name num) local)
  (Scope modname (hash-set innermost name num)))

(define (scope-set-modname scope modname)
  (match-define (Scope _ innermost) scope)
  (Scope modname innermost))

(struct Scope (modname innermost) #:transparent)
; innermost maps names to the innermost bound local number

(define (empty-scope [modname #f])
  (Scope modname (hash)))

(module+ test
  (require rackunit)
  (check-equal? (parse 'x) (Global #f 'x))
  (check-equal? (parse 'q.x) (Global 'q 'x))
  (check-equal? (parse '(lambda (x) x)) (Func (list (Local 'x 0)) (Local 'x 0)))
  (check-equal? (parse '(lambda (x) (lambda (x) x))) (Func (list (Local 'x 0))
                                                           (Func (list (Local 'x 1))
                                                                 (Local 'x 1))))
  (check-equal? (parse '(module q (= x y)))
                (Module 'q (list (Def (Global 'q 'x) (Global 'q 'y)))))
  (check-equal? (parse '(let ([x 1] [y 2]) z))
                (parse '((lambda (x y) z) 1 2)))
  (check-equal? (parse '(let* ([x 1] [y 2]) z))
                (parse '(let ([x 1]) (let ([y 2]) z))))
  (check-equal? (parse '(cond [1 2] [3 4] [else 5]))
                (parse '(if 1 2 (if 3 4 5))))
  (check-equal? (parse '(cond [1 2] [3 4]))
                (parse '(if 1 2 (if 3 4 (error "no case")))))
  (check-equal? (parse '(begin 1 2 3))
                (parse '(let* ([_ 1] [_ 2]) 3))))
