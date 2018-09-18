#lang eopl

(require rackunit "2.26.rkt")
(require rackunit/text-ui)

(define input-tree
  (red-node
    (blue-node (list (leaf-node 26) (leaf-node 12)))
    (red-node
      (leaf-node 11)
      (blue-node (list (leaf-node 117) (leaf-node 14))))))

(define expected-tree
  (red-node
    (blue-node (list (leaf-node 1) (leaf-node 1)))
    (red-node (leaf-node 2) (blue-node (list (leaf-node 2) (leaf-node 2))))))

(define red-blue-tree-test
  (test-suite
    "Tests for red blue tree"
    (check-equal? (mark-leaves-with-red-depth input-tree) expected-tree)
    ))

(run-tests red-blue-tree-test)
