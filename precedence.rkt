#lang racket

(provide empty-graph
         add-fact
         graph
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

 (Graph representatives ; hash( vertex -> vertex )
        neighbors ; hash( vertex -> vertex )
        assocs ; setof AssocLeft/AssocRight
        ))

(define Fact?
  (or/c Tighter? Equal? AssocLeft? AssocRight?))


(define/contract (empty-graph) (-> Graph?)
  (Graph (hash)
         (hash)
         (set)))

(define/contract (add-fact g fact) (-> Graph? Fact? Graph?)
  (match-define (Graph representatives neighbors assocs) g)
  (match fact
    [(AssocLeft op) (if (set-member? assocs (AssocRight op))
                        (error 'precedence "~v cannot be both left and right associative" op)
                        (Graph representatives neighbors (set-add assocs fact)))]
    [(AssocRight op) (if (set-member? assocs (AssocLeft op))
                         (error 'precedence "~v cannot be both left and right associative" op)
                         (Graph representatives neighbors (set-add assocs fact)))]
    [(Tighter a b)
     ; new edge a -> b creates a cycle:
     ; - if path b ---> a already exists
     ; - if a == b is already known
     (let ([a-rep (hash-ref representatives a a)]
           [b-rep (hash-ref representatives b b)])
       (cond
         [(equal? a-rep b-rep) (error 'precedence "cycle with ~v and ~v" a b)]
         [(can-reach? neighbors b-rep a-rep) (error 'precedence "cycle with ~v and ~v" a b)]
         [else
          (Graph representatives
                 (hash-set neighbors
                           a-rep
                           (set-add (hash-ref neighbors a-rep '())
                                    b-rep))
                 assocs)]))]
    [(Equal a b)
     ; new equivalence a == b creates a cycle if a and b are already different,
     ; that is, if path b ---> a OR path a ---> b already exists
     (let ([a-rep (hash-ref representatives a a)]
           [b-rep (hash-ref representatives b b)])
       (if (or (can-reach? neighbors b-rep a-rep)
               (can-reach? neighbors a-rep b-rep))
           (error 'precedence "cycle with ~v and ~v" a b)
           ; TODO generate test case about why updating neighbors is important
           ; use A as the new representative: make B point to A everywhere
           (let* ([representatives (hash-set representatives b a-rep)]
                  [representatives (hash-map-values (match-lambda
                                                      [(== b-rep) a-rep]
                                                      [v v])
                                                    representatives)]
                  ; any values in neighbors need to be updated
                  ; any keys need to be updated and merged!
                  ;    a -> [q, z]
                  ;    b -> [w, z]
                  ;  a == b
                  ;    a -> [q, w, z]
                  ; 1. update values
                  [neighbors (hash-map-values (lambda (ns)
                                                (map (match-lambda
                                                       [(== b-rep) a-rep]
                                                       [v v])
                                                     ns))
                                              neighbors)]
                  ; 2. update keys
                  [neighbors (hash-set neighbors
                                       a-rep
                                       (set-union (hash-ref neighbors a-rep '())
                                                  (hash-ref neighbors b-rep '())))]
                  [neighbors (hash-remove neighbors b-rep)])
             (Graph representatives
                    neighbors
                    assocs))))]))

(define (can-reach? neighbors start end [seen (set)])
  ; does a nonempty path exist from start to end?
  (cond
    [(set-member? seen start) #false]
    [else (let ([seen (set-add seen start)])
            (for/or ([n (hash-ref neighbors start '())])
              (or (equal? n end)
                  (can-reach? neighbors n end seen))))]))
(module+ test
  (let ()
    (define g (hash 'a '(x)
                    'x '(x y)
                    'y '(z)))
    ; self edges must be explicit
    (check-equal? (can-reach? g 'a 'a) #false)
    (check-equal? (can-reach? g 'x 'x) #true)
    ; simple edges
    (check-equal? (can-reach? g 'a 'x) #true)
    (check-equal? (can-reach? g 'x 'y) #true)
    (check-equal? (can-reach? g 'y 'z) #true)
    ; no edge
    (check-equal? (can-reach? g 'x 'a) #false)
    ; transitive cases
    (check-equal? (can-reach? g 'x 'z) #true)
    (check-equal? (can-reach? g 'a 'z) #true)))

(define (hash-map-values f h)
  (for/hash ([{k v} (in-hash h)])
    (values k (f v))))

(define (graph . facts)
    (for/fold ([g (empty-graph)]) ([f facts])
      (add-fact g f)))

(module+ test
  
  (check-exn exn:fail?
             (lambda () (graph (AssocLeft 'x)
                               (AssocRight 'x)))
             "left and right")

  ; directed cycles
  (check-exn exn:fail?
             (lambda () (graph (Tighter 'x 'x)))
             "cycle")
  (check-exn exn:fail?
             (lambda () (graph (Tighter 'x 'y)
                               (Tighter 'y 'x)))
             "cycle")
  (check-exn exn:fail?
             (lambda () (graph (Tighter 'x 'y)
                               (Tighter 'y 'z)
                               (Tighter 'z 'x)))
             "cycle")

  ; cycle involving an Equal edge
  (check-exn exn:fail?
             (lambda () (graph (Tighter 'x 'y)
                               (Equal 'x 'y)))
             "cycle")


  ;;
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
  (match-define (Graph representatives neighbors _) graph)
  (define left-rep (hash-ref representatives left left))
  (define right-rep (hash-ref representatives right right))
  (cond
    ; 1. are the operators in the same equivalence class?
    [(equal? left-rep right-rep) 'equal]
    ; 2. is one equivalence class adjacent to the other?
    [(set-member? (hash-ref neighbors left-rep '()) right-rep) 'left]
    [(set-member? (hash-ref neighbors right-rep '()) left-rep) 'right]
    [else #false]))

(define/contract (query-assoc graph op) (-> Graph? any/c (or/c 'left 'right #false))
  (match-define (Graph _ _ assocs) graph)
  (cond
    [(set-member? assocs (AssocLeft op)) 'left]
    [(set-member? assocs (AssocRight op)) 'right]
    [else #false]))


(module+ test


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

  (define g2 (add-fact g1 (Tighter '^ '+)))

  (check-equal? (resolve-precedence g2 '+ '^) 'right)
  (check-equal? (resolve-precedence g2 '^ '+) 'left)


  ;;
  )
