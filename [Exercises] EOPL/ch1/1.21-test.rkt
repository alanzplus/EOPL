#lang eopl

(require rackunit "1.21.rkt")
(require rackunit/text-ui)

(define product-test
  (test-suite
    "Tests for (product sos1 sos2"
    (check-equal?
      (product '(a b c) '(x y))
      '((a x) (a y) (b x) (b y) (c x) (c y)))))

(run-tests product-test)
