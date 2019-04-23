#lang eopl

(require rackunit "2.24.rkt")
(require rackunit/text-ui)

(define tree
  (interior-node 'a (leaf-node 3) (leaf-node 4)))

(define bintree-to-list-test
  (test-suite
    "Tests for bintree-to-list"
    (check-equal?
      (bintree-to-list tree)
      '(interior-node a (leaf-node 3) (leaf-node 4)))))

(run-tests bintree-to-list-test)
