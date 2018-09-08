#lang eopl

(require "bintree.rkt")
(require rackunit "1.35.rkt")
(require rackunit/text-ui)

(define input-tree
  (interior-node 'foo
    (interior-node 'bar
      (leaf 26)
      (leaf 12))
    (interior-node 'baz
      (leaf 11)
      (interior-node 'quux
        (leaf 117)
        (leaf 14)))))

(define expected-tree
  (interior-node 'foo
    (interior-node 'bar
      (leaf 0)
      (leaf 1))
    (interior-node 'baz
      (leaf 2)
      (interior-node 'quux
        (leaf 3)
        (leaf 4)))))

(define number-leaves-test
  (test-suite
    "Tests for number-leaves"
    (check-equal?
      (tree-equal? (number-leaves input-tree) expected-tree)
      #t)))

(run-tests number-leaves-test)
