#lang eopl

(require rackunit "vector-sum.rkt")
(require rackunit/text-ui)

(define vector-sum-test
  (test-suite
    "Tests fro vector-sum.rkt"
    (check-equal? (vector-sum (vector 1 2 3)) 6)))

(run-tests vector-sum-test)
