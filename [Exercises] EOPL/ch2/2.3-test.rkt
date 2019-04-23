#lang eopl

(require rackunit "2.3.rkt")
(require rackunit/text-ui)

(define diff-tree-test
  (test-suite
    "Tests for diff tree"
    (check-equal?
      (diff (one) (diff (one) (one))) (one))
    (check-equal?
      (diff (diff (one) (one)) (one))
      -1)))

(run-tests diff-tree-test)
