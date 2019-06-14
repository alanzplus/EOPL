#lang eopl

(require rackunit "1.25.rkt")
(require rackunit/text-ui)

(define exists-test
  (test-suite
    "Tests for (exists? pred lst)"
    (check-equal? (exists? number? '(a b c 3 e)) #t)
    (check-equal? (exists? number? '(a b c d e)) #f)))

(run-tests exists-test)
