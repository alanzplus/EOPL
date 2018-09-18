#lang eopl

(require rackunit "2.25.rkt")
(require rackunit/text-ui)

(define tree-foo
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))

(define tree-bar
  (interior-node 'bar (leaf-node -1) tree-foo))

(define tree-baz
  (interior-node 'baz tree-bar (leaf-node 1)))

(define max-interior-test
  (test-suite
    "Tests for max interior"
    (check-equal? (max-interior tree-foo) 'foo)
    (check-equal? (max-interior tree-bar) 'foo)
    (check-equal? (max-interior tree-baz) 'baz)))

(run-tests max-interior-test)
