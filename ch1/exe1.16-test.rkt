#lang eopl

(require rackunit "exe1.16.rkt")
(require rackunit/text-ui)

(define invert-test
  (test-suite
    "Tests for invert"
    (check-equal? (invert '((a 1) (b 2))) '((1 a) (2 b)))
    (check-equal? (invert '()) '())))

(run-tests invert-test)
