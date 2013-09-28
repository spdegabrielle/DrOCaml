#lang scheme

(define (fact n)
  (if (< n 2)
      1
      (* n (fact (- n 1)))))

(fact 5)

(define (fact2 n)
  (if (< n 2)
      1
      (* n (fact2 (- n 1)))))

(fact2 5)

