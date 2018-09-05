#lang eopl

(require rackunit "exe1.26.rkt")
(require rackunit/text-ui)

(define up-test
  (test-suite
    "Tests for (up lst)"
    (check-equal? (up '((1 2) (3 4))) '(1 2 3 4))
    (check-equal? (up '((x (y)) z)) '(x (y) z))))

(run-tests up-test)
