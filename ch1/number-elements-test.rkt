#lang eopl

(require rackunit "number-elements.rkt")
(require rackunit/text-ui)

(define number-elements-test
  (test-suite
    "Tests for number-elments.rkt"
    (check-equal? (number-elements '(v0 v1 v2)) '((v0 0) (v1 1) (v2 2)))))

(run-tests number-elements-test)
