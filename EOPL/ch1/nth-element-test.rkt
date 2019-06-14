#lang eopl

(require rackunit "nth-element.rkt")
(require rackunit/text-ui)

(define nth-element-test
  (test-suite
    "Tests for nth-element.rkt"
    (check-equal? (nth-element '(a b c) 0) 'a)
    (check-equal? (nth-element '(a b c) 1) 'b)
    (check-equal? (nth-element '(a b c) 2) 'c)))

(run-tests nth-element-test)
