#lang eopl

(require rackunit "exe1.24.rkt")
(require rackunit/text-ui)

(define every-test
  (test-suite
    "Tests for (every? pred lst)"
    (check-equal? (every? number? '()) #t)
    (check-equal? (every? number? '(a b c 3 e)) #f)
    (check-equal? (every? number? '(1 2 3 5 4)) #t)))

(run-tests every-test)
