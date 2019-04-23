#lang eopl

(require "1.31.rkt")
(require rackunit "1.33.rkt")
(require rackunit/text-ui)

(define input-tree
  (interior-node
    'red
    (interior-node
      'bar
      (leaf 26)
      (leaf 12))
    (interior-node
      'red
      (leaf 11)
      (interior-node
        'quux
        (leaf 117)
        (leaf 14)))))

(define output-tree
  (interior-node
    'red
    (interior-node
      'bar
      (leaf 1)
      (leaf 1))
    (interior-node
      'red
      (leaf 2)
      (interior-node
        'quux
        (leaf 2)
        (leaf 2)))))

(define mark-leaves-with-red-depth-test
  (test-suite
    "Tests for mark-leaves-with-red-depth"
    (check-equal?
      (tree-equal?
        (mark-leaves-with-red-depth input-tree)
        output-tree)
      #t)))

(run-tests mark-leaves-with-red-depth-test)
