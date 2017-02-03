#lang s-exp "./vanilla.rkt"


(def main
  (lambda (input)
    (let ([inp (parse input)])
      (show (list
             (measure (interp initial-state inp))
             ;;(measure (find-first-dup (uniq-adjacent (map state-loc (trace initial-state inp)))))
             )))))

(def measure
  (lambda (state)
    (+ (abs (first state))
       (abs (first (rest state))))))

(def initial-state (list 0 0 "N"))

(def state-loc (lambda (s) (list (first s) (first (rest s)))))

(def interp (lambda (state instructions) (foldl step state instructions)))

(def step
  (lambda (state instruction)
    (cond
      [(equal? instruction "R") (turn-R state)]
      [(equal? instruction "L") (turn-L state)]
      [else (let* ([x (first state)]
                   [y (first (rest state))]
                   [dir (first (rest (rest state)))]
                   [i instruction])
              (cond
                [(equal? dir "N") (list x (+ y i) dir)]
                [(equal? dir "S") (list x (- y i) dir)]
                [(equal? dir "E") (list (+ x i) y dir)]
                [(equal? dir "W") (list (- x i) y dir)]
                [else (error "bad dir")]))])))

(def turn-R
  (lambda (state)
    (let* ([x (first state)]
           [y (first (rest state))]
           [dir (first (rest (rest state)))])
      (cond
        [(equal? dir "N") (list x y "E")]
        [(equal? dir "E") (list x y "S")]
        [(equal? dir "S") (list x y "W")]
        [(equal? dir "W") (list x y "N")]
        [else (error "bad dir")]))))

(def turn-L
  (lambda (state)
    (let* ([x (first state)]
           [y (first (rest state))]
           [dir (first (rest (rest state)))])
      (cond
        [(equal? dir "N") (list x y "W")]
        [(equal? dir "W") (list x y "S")]
        [(equal? dir "S") (list x y "E")]
        [(equal? dir "E") (list x y "N")]
        [else (error "bad dir")]))))

(def parse
  (lambda (str)
    (concat (map parse-tok (split str ", ")))))

(def parse-tok
  (lambda (tok)
    (cons (slice tok 0 1) ; the letter part: "L" or "R"
          ; unary-encode the number part
          (replicate (parse-int (slice tok 1 (length tok)))
                     1))))

(def abs
  (lambda (n)
    (if (< n 0)
        (- n)
        n)))

; note: uses haskell infix / arg order
(def foldl
  (lambda (binop base lst)
    (if (empty? lst)
        base
        (foldl binop
               (binop base (first lst))
               (rest lst)))))

(def map
  (lambda (f lst)
    (if (empty? lst)
        lst
        (cons (f (first lst))
              (map f (rest lst))))))
