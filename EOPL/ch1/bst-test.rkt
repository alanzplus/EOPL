#lang eopl

(require rackunit "bst.rkt")
(require rackunit/text-ui)

(define atree
  (bst
    3
    (bst 1 '() '())
    (bst 4 '() '())))

(define bst-test
  (test-suite
    "Tests for bst-test"
    (check-equal? (null-node? '()) #t)
    (check-equal? (val-of-node '()) '())
    (check-equal? (val-of-node (ltree atree)) 1)
    (check-equal? (val-of-node (rtree atree)) 4)
    (check-equal? (val-of-node atree) 3)))

(run-tests bst-test)
