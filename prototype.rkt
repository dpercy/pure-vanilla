#lang racket

; Do not confuse object-language values with meta-language values.
; An object-language value satisfies Value?.
(struct Value () #:transparent)

(struct Leaf Value ([value : (U Boolean
                                String
                                Symbol
                                Number
                                )]) #:transparent)

(define-type List (U Empty Cons))
(struct Empty Value () #:transparent)
(struct Cons Value ([head : Value]
                    [tail : List]) #:transparent)

(struct Closure Value
  ([params : ])
  #:transparent)



(define-type MetaLangValue
  (U Boolean
     String
     Symbol
     Number
     (Listof MetaLangValue)
     ; TODO functions?
     ))

(define (meta->object [value : MetaLangValue]) : Value
  (match value
    [(or (? boolean?)
         (? string?)
         (? symbol?)
         (? number?)) (Leaf value)]
    [(or (? empty?)
         (? cons?)) (foldr Cons (Empty) (map meta->object value))]))

(: object->meta (case-> (-> List (Listof MetaLangValue))
                        (-> Value MetaLangValue)))
(define (object->meta value)
  (match value
    [(Leaf v) v]
    [(Empty) '()]
    [(Cons a b) (cons (object->meta a)
                      (object->meta b))]))
