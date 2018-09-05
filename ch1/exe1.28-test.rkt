#lang eopl

(require rackunit "exe1.28.rkt")
(require rackunit/text-ui)

(define merge-test
  (test-suite
    "Testse for (merge loi1 loi2)"
    (check-equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
    (check-equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))))

(run-tests merge-test)
