#lang eopl

(require "bintree.rkt")
(require rackunit "1.36.rkt")
(require rackunit/text-ui)

(define number-elements-test
  (test-suite
    "Tests for number-elements"
    (check-equal?
      (number-elements '(a b c d))
      '((0 a) (1 b) (2 c) (3 d)))))

(run-tests number-elements-test)
