#lang racket

(provide empty-graph
         add-fact
         Tighter Tighter?
         Equal Equal?
         AssocLeft AssocLeft?
         AssocRight AssocRight?
         Graph?
         Fact?
         resolve-precedence
         )

#;(
   #|
   input: a set of rules like
   - "* binds tighter than +"
   - "+ is left associative"
   - "+ and - have equal precedence"

   output: the answer to queries like
   - "how do I paren `_ * _ + _`"
   - "how do I paren `_ + _ * _`"

   OR, if there is a contradiction in the input rules,
   it points this out.
   - "+ binds tighter than -" ; "+ and - have equal precedence" contradict



   Note:
   - whether + or - binds tighter depends on their order:
   .  the left one always wins.
   .  So you can't say operators are DAG nodes.
   .  At most, maybe operator-sets are DAG nodes.

   - Should "binds tighter than" be transitive?
   "Parsing Mixfix Operators"
   http://www.cse.chalmers.se/~nad/publications/danielsson-norell-mixfix.pdf
   Table 1:
   prec     assoc      result
   + < *               a + (b * c)
   * < +               (a + b) * c
   + = *    both left  (a + b) * c
   + = *    both right a + (b * c)
   none of the above   parse error


   partial order vs DAG
   - note DAG is "A" acyclic: system can still prevent bullshit
   |#
   )

(require "constructor.rkt")
(module+ test (require rackunit))

(tag-structs
 "Precedence"

 (Tighter a b) ; fact about two ops
 (Equal a b) ; fact about two ops

 (AssocLeft a) ; fact about one op
 (AssocRight a) ; fact about one op

 (Graph facts)
 )

(define Fact?
  (or/c Tighter? Equal? AssocLeft? AssocRight?))


(define/contract (empty-graph) (-> Graph?)
  (Graph (set)))

(define/contract (add-fact g fact) (-> Graph? Fact? Graph?)
  (match g
    [(Graph facts) (checked-Graph (set-add facts fact))]))

(define/contract (checked-Graph facts) (-> (set/c Fact?) Graph?)
  ; checks the graph for consistency:
  ; - each operator must have at most one associativity
  ; - cycles are not allowed
  (check-dup-assoc! facts)
  (check-cycles! facts)
  (Graph facts))

(define (check-dup-assoc! facts)
  (for ([fact facts])
    (match fact
      [(AssocLeft op) #:when (set-member? facts (AssocRight op))
       (error 'precedence "~v cannot be both left and right associative" op)]
      [_ (void)])))
(module+ test
  (check-exn exn:fail?
             (lambda () (check-dup-assoc! (set (AssocLeft 'x)
                                               (AssocRight 'x))))
             "left and right")
  (check-not-exn (lambda () (check-dup-assoc! (set (AssocLeft 'x)))))
  (check-not-exn (lambda () (check-dup-assoc! (set (AssocRight 'x)))))
  (check-not-exn (lambda () (check-dup-assoc! (set (AssocRight 'x) (AssocRight 'x)))))
  (check-not-exn (lambda () (check-dup-assoc! (set (AssocLeft 'y) (AssocRight 'z))))))


(define (check-cycles! facts)
  ; 1. choose a representative operator from each DAG node
  (define representative
    (for/fold ([h (hash)]) ([fact facts]
                            #:when (Equal? fact))
      (match-define (Equal a b) fact)
      (let ([a (hash-ref h a a)]
            [b (hash-ref h b b)])
        ; update the hash table with:
        ; a -> b
        ; forall existing rules x -> a; put x -> b.
        (let ([h (hash-set h a b)])
          (for/hash ([{k v} (in-hash h)])
            (values k (if (equal? v a)
                          b
                          v)))))))
  ; 2. rewrite edges using representative operators
  (define edges
    (for/set ([fact facts]
              #:when (Tighter? fact))
      (match fact
        [(Tighter a b)
         (Tighter (hash-ref representative a a)
                  (hash-ref representative b b))])))
  (define neighbors
    (for/fold ([h (hash)]) ([e edges])
      (match e
        [(Tighter a b) (hash-set h a
                                 (cons b
                                       (hash-ref h a '())))])))
  ; 3. cycle detect the graph of operators
  (for ([vertex (in-hash-keys neighbors)])
    ; TODO memoize recur!
    (let recur ([vertex vertex]
                [seen (set)])
      (if (set-member? seen vertex)
          (error 'precedence "cycle involving ~v" vertex)
          (let ([seen (set-add seen vertex)])
            (for ([neighbor (hash-ref neighbors vertex '())])
              (recur neighbor seen)))))))
(module+ test
  ; directed cycles
  (check-exn exn:fail?
             (lambda () (check-cycles! (set (Tighter 'x 'x))))
             "cycle")
  (check-exn exn:fail?
             (lambda () (check-cycles! (set (Tighter 'x 'y)
                                            (Tighter 'y 'x))))
             "cycle")
  (check-exn exn:fail?
             (lambda () (check-cycles! (set (Tighter 'x 'y)
                                            (Tighter 'y 'z)
                                            (Tighter 'z 'x))))
             "cycle")

  ; cycle involving an Equal edge
  (check-exn exn:fail?
             (lambda () (check-cycles! (set (Tighter 'x 'y)
                                            (Equal 'x 'y))))
             "cycle")


  )


(define/contract (resolve-precedence graph left right) (-> Graph? any/c any/c
                                                           (or/c 'left 'right #false))
  (match (query-prec graph left right)
    ['left 'left]
    ['right 'right]
    [#false #false]
    ['equal (match (list (query-assoc graph left) (query-assoc graph right))
              [(list 'left 'left) 'left]
              [(list 'right 'right) 'right]
              [_ #false])]))

(define/contract (query-prec graph left right) (-> Graph? any/c any/c
                                                   (or/c 'left 'right 'equal #false))
  'TODO
  ; 1. are the operators in the same equivalence class?
  ; 2. is one equivalence class adjacent to the other?
  )

(define/contract (query-assoc graph op) (-> Graph? any/c (or/c 'left 'right #false))
  'TODO
  )

#;
(module+ test
  (define (graph . facts)
    (for/fold ([g (empty-graph)]) ([f facts])
      (add-fact g f)))

  (define g1
    (graph (AssocLeft '+)
           (AssocLeft '-)
           (Equal '+ '-)

           (Tighter '* '+)
           (Equal '* '/)

           (AssocRight '^)
           (Tighter '^ '/)

           ; note there is no explicit rule for ^ with +
           ))

  (check-equal? (resolve-precedence g1 '+ '-) 'left)
  (check-equal? (resolve-precedence g1 '- '+) 'left)

  (check-equal? (resolve-precedence g1 '+ '+) 'left)
  (check-equal? (resolve-precedence g1 '- '-) 'left)

  (check-equal? (resolve-precedence g1 '- '/) 'right)
  (check-equal? (resolve-precedence g1 '/ '-) 'left)

  (check-equal? (resolve-precedence g1 '+ '^) #false)
  (check-equal? (resolve-precedence g1 '^ '+) #false)

  (check-equal? (resolve-precedence g1 '^ '^) 'right)

  (define g2 (add-fact g1 (Tighter '+ '^)))

  (check-equal? (resolve-precedence g2 '+ '^) 'right)
  (check-equal? (resolve-precedence g2 '^ '+) 'left)


  ;;
  )
