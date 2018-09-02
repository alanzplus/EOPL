#lang eopl

(require rackunit "remove.rkt")
(require rackunit/text-ui)

(define remove-test
  (test-suite
    "Tests for remove.rkt"
    (check-equal? (remove 'a '(a b c)) '(b c))
    (check-equal? (remove 'b '(e f g)) '(e f g))
    (check-equal? (remove 'a4 '(c1 a4 c1 a4)) '(c1 c1))
    (check-equal? (remove 'x '()) '())))

(run-tests remove-test)
