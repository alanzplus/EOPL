#lang eopl

(require rackunit "exe1.29.rkt")
(require rackunit/text-ui)

(define sort-test
  (test-suite
    "Tests for (sort loi)"
    (check-equal? (sort '(1)) '(1))
    (check-equal? (sort '()) '())
    (check-equal? (sort '(8 2 5 2 3)) '(2 2 3 5 8))))

(run-tests sort-test)
