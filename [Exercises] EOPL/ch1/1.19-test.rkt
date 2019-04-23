#lang eopl

(require rackunit "1.19.rkt")
(require rackunit/text-ui)

(define list-set-test
  (test-suite
    "Tests for (list-set lst n x)"
    (check-equal? (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
    (check-equal? (list-set '(a b c d) 3 '(1 5 10)) '(a b c (1 5 10)))))

(run-tests list-set-test)
