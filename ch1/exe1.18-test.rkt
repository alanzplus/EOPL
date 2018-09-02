#lang eopl

(require rackunit "exe1.18.rkt")
(require rackunit/text-ui)

(define swapper-test
  (test-suite
    "Tests for (swapper s1 s2 slist)"
    (check-equal? (swapper 'a 'd '(a b c d)) '(d b c a))
    (check-equal? (swapper 'a 'd '(a d () c d)) '(d a () c a))
    (check-equal?
      (swapper 'x 'y '((x) y (z (x))))
      '((y) x (z (y))))))

(run-tests swapper-test)
