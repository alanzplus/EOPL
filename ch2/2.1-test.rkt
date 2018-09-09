#lang eopl

(require rackunit "2.1.rkt")
(require rackunit/text-ui)

(define bigits-test
  (test-suite
    "Tests for bigits"
    (check-equal? (zero) '())
    (check-equal? (is-zero? (zero)) #t)
    (check-equal? (successor (zero)) '(1))
    (check-equal? (predecessor (successor (zero))) (zero))))

(run-tests bigits-test)
