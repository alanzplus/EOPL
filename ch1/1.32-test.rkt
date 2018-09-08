#lang eopl

(require "1.31.rkt")
(require rackunit "1.32.rkt")
(require rackunit/text-ui)

(define input-tree
  (interior-node 'root
    (interior-node 'left
      (leaf 26)
      (leaf 25))
    (interior-node 'right
      (leaf 20)
      '())))

(define double-tree-tree
  (interior-node 'root
    (interior-node 'left
      (leaf 52)
      (leaf 50))
    (interior-node 'right
      (leaf 40)
      '())))

(define double-tree-test 
  (test-suite
    "Tests for double tree"
    (check-equal? (tree-equal? (double-tree input-tree) double-tree-tree) #t)))

(run-tests double-tree-test)
