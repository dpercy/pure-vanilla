#lang racket

(module+ test (require rackunit))
(require (only-in racket/syntax format-symbol))

; http://conal.net/talks/teaching-new-tricks-to-old-programs.pdf

; data constructors
(struct Color (r g b) #:transparent)
(struct Pair (left right) #:transparent)

; types
(struct TF (type function) #:transparent
  ; "typed function"
  #:property prop:procedure (struct-field-index function))

(struct F (input-type output-type) #:transparent)
(struct P (left-type right-type) #:transparent)
(define type?
  (flat-rec-contract
   type?
   (or/c "Number"
         "String"
         "Color"
         (struct/c P type? type?) ; pair
         (struct/c F type? type?) ; function
         symbol? ; type variable
         )))

(define (typeof v)
  (match v
    [(? number?) "Number"]
    [(? string?) "String"]
    [(? Color?) "Color"]
    [(Pair a b) (P (typeof a) (typeof b))]
    [(TF type _) type]
    [_ (error 'typeof "no case for ~v" v)]))

(define subst? (hash/c symbol? type?))

; rename all the type variables consistently with an existing subst,
; and update the subst with any newly chosen names.
(define (rename type [subst (make-hash)])
  (define (r type) (rename type subst))
  (match type
    [(? symbol?) (hash-ref! subst type
                            (lambda ()
                              (gensym
                               (regexp-replace "[0-9]*$"
                                               (symbol->string type)
                                               ""))))]
    [(F a b) (F (r a) (r b))]
    [(P a b) (P (r a) (r b))]
    [(? string?) type]))
(module+ test
  (check-match (rename (F 'a 'b))
               (F a b)
               (not (equal? a b)))
  (check-match (rename (F 'a 'a))
               (F a a)))

(define (apply-subst subst type)
  (define (r type) (apply-subst subst type))
  (match type
    [(? symbol?) (if (hash-has-key? subst type)
                     (r (hash-ref subst type))
                     type)]
    [(F a b) (F (r a) (r b))]
    [(P a b) (P (r a) (r b))]
    [(? string?) type]))

; take two types and finds a substitution that makes them equal.
; assumes A and B have disjoint variables.
(define (unify A B [subst (make-hash)])
  (define (lookup x) (hash-ref subst x))
  (define (bound? x) (and (symbol? x) (hash-has-key? subst x)))
  (define (free? x) (and (symbol? x) (not (bound? x))))
  (let unify ([A A]
              [B B])
    (define (fail!) (error 'unify "failed to unify ~v with ~v" A B))
    (match* {A B}
      [{(? string?) (? string?)} (if (equal? A B)
                                     (void)
                                     (fail!))]
      [{(F A0 A1) (F B0 B1)} (begin
                               (unify A0 B0)
                               (unify A1 B1))]
      [{(P A0 A1) (P B0 B1)} (begin
                               (unify A0 B0)
                               (unify A1 B1))]
      [{(? free?) _} (hash-set! subst A B)]
      [{_ (? free?)} (hash-set! subst B A)]
      [{(? bound?) _} (unify (lookup A) B)]
      [{_ (? bound?)} (unify A (lookup B))]
      [{_ _} (fail!)]))
  subst)
(module+ test

  ; mismatched concrete types
  (check-exn exn:fail? (lambda () (unify "String" "Number")))
  (check-exn exn:fail? (lambda () (unify (F "Number" "Number") (F "Number" "String"))))


  (check-match (unify 'a 'b)
               (hash-table ['a 'b]))
  (check-match (unify 'a "Number")
               (hash-table ['a "Number"]))
  (check-match (unify "Number" 'a)
               (hash-table))

  ; one side gets mapped to a polymorphic type
  (check-match (unify 'a (F 'x 'y))
               (hash-table ['a (F 'x 'y)]))

  ; equality constraint fails
  (check-exn exn:fail?
             (lambda ()
               (unify (P 'a 'a)
                      (P "Number" "String"))))


  ; A and B first get renamed, then later a constraint fails
  (check-exn exn:fail?
             (lambda ()
               (unify (P 'a (P 'a "String"))
                      (P 'A (P "Number" 'A)))))
  ; ... or succeeds
  (check-match (unify (P 'a (P 'a 'y))
                      (P 'A (P 'X 'A)))
               (hash-table ['a 'A]
                           ['X 'a]
                           ['y 'A]))
  (check-match (unify (P 'a (P 'a "Number"))
                      (P 'A (P 'X 'A)))
               (hash-table ['a 'A]
                           ['X 'a]
                           ['A "Number"])))


(define (Const b)
  (TF (F 'a (typeof b))
      (lambda (a) b)))

(define Id
  (TF (F 'a 'a)
      (lambda (a) a)))

(define (Compose g f)
  ; need to ensure:
  ;  - g is a function
  ;  - f is a function
  ;  - f's output is compatible with g's input
  (define tg (rename (typeof g)))
  (define tf (rename (typeof f)))

  (define subst (make-hash))
  (unify tg (F 'g-in 'g-out) subst)
  (unify tf (F 'f-in 'f-out) subst)
  (unify 'g-in 'f-out subst)

  (TF (apply-subst subst (F 'f-in 'g-out))
      (lambda (a) (g (f a)))))

(module+ test
  ; compose some concrete functions
  (check-equal? (typeof (Compose (TF (F "Number" "String") #f)
                                 (TF (F "Color" "Number") #f)))
                (F "Color" "String"))

  ; compose Id with a concrete function
  (check-equal? (typeof (Compose Id
                                 (TF (F "Color" "Number") #f)))
                (F "Color" "Number"))
  (check-equal? (typeof (Compose (TF (F "Color" "Number") #f)
                                 Id))
                (F "Color" "Number"))

  ; compose a non-function
  (check-exn exn:fail?
             (lambda ()
               (Compose (TF Id 3)))
             "failed to unify")

  ;;
  )

(define (Fork f g)
  ; need to ensure:
  ;  - f is a function
  ;  - g is a function
  ;  - f and g accept the same inputs

  (define tf (rename (typeof f)))
  (define tg (rename (typeof g)))

  (define subst (make-hash))
  (unify tf (F 'f-in 'f-out) subst)
  (unify tg (F 'g-in 'g-out) subst)
  (unify 'f-in 'g-in subst)

  (TF (apply-subst subst (F 'in (P 'f-out 'g-out)))
      (lambda (a) (Pair (f a) (g a)))))

(define (Curry f)
  ; need to ensure:
  ;  - f is a function
  ;  - f can accept pairs

  (define subst (unify (rename (typeof f))
                       (F (P 'in-left 'in-right) 'out)))

  (TF (apply-subst subst (F 'in-left (F 'in-right 'f-out)))
      (lambda (a) (lambda (b) (f (Pair a b))))))

(define Apply
  (TF (F (P (F 'a 'b) 'a) 'b)
      (match-lambda [(Pair f a) (f a)])))


; arith prims

(define mulC (TF (F (P "Number" "Number") "Number")
                 (match-lambda [(Pair a b) (* a b)])))
(define addC (TF (F (P "Number" "Number") "Number")
                 (match-lambda [(Pair a b) (+ a b)])))
(define cosC (TF (F "Number" "Number")
                 (lambda (a) (cos a))))
(define sinC (TF (F "Number" "Number")
                 (lambda (a) (sin a))))

; pair prims

(define exl (TF (F (P 'a 'b) 'a)
                Pair-left))
(define exr (TF (F (P 'a 'b) 'b)
                Pair-right))


; Examples
(module+ test


  (define sqr (Compose mulC (Fork Id Id)))
  (check-equal? (sqr 4) 16)

  (define magSqr (Compose addC
                          (Fork (Compose mulC (Fork exl exl))
                                (Compose mulC (Fork exr exr)))))
  (check-equal? (magSqr (Pair 3 4)) 25)

  (define cosSinProd (Compose (Fork cosC sinC) mulC))
  (check-equal? (cosSinProd (Pair 2 3)) (Pair (cos 6) (sin 6)))

  )
