#lang eopl

(require "bst.rkt")
(require rackunit "1.34.rkt")
(require rackunit/text-ui)

(define input-tree
  (bst 14
    (bst 7
      '()
      (bst 12 '() '()))
    (bst 26
      (bst 20
        (bst 17 '() '())
        '())
      (bst 31 '() '()))))

(define path-test
  (test-suite
    "Tests for path"
    (check-equal? (path 17 input-tree) '(right left left))))

(run-tests path-test)
