#lang eopl

(require rackunit "list-sum.rkt")
(require rackunit/text-ui)

(define list-sum-test
  (test-suite
    "Tests for list-sum.rkt"
    (check-equal? (list-sum (list 1 2 3)) 6)))

(run-tests list-sum-test)
