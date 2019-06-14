#lang eopl

(require rackunit "1.30.rkt")
(require rackunit/text-ui)

(define sort-test
  (test-suite
    "Tests for (sort/predict pred loi)"
    (check-equal? (sort < '(8 2 5 2 3)) '(2 2 3 5 8))
    (check-equal? (sort > '(8 2 5 2 3)) '(8 5 3 2 2))))

(run-tests sort-test)

