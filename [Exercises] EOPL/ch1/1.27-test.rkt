#lang eopl

(require rackunit "1.27.rkt")
(require rackunit/text-ui)

(define flatten-test
  (test-suite
    "Tests for (flatten slist)"
    (check-equal? (flatten '(a b c)) '(a b c))
    (check-equal? (flatten '((a) () (b ()) (c))) '(a b c))
    (check-equal? (flatten '((a b) c (((d))) e)) '(a b c d e))
    (check-equal? (flatten '(a b (() (c)))) '(a b c))))

(run-tests flatten-test)
