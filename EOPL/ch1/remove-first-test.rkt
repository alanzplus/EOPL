#lang eopl

(require rackunit "remove-first.rkt")
(require rackunit/text-ui)

(define remove-first-test
  (test-suite
    "Tests for remove-first.rkt"
    (check-equal? (remove-first 'a '(a b c)) '(b c))
    (check-equal? (remove-first 'b '(e f g)) '(e f g))
    (check-equal? (remove-first 'a4 '(c1 a4 c1 a4)) '(c1 c1 a4))
    (check-equal? (remove-first 'x '()) '())))

(run-tests remove-first-test)
