#lang eopl

(require rackunit "bintree.rkt")
(require rackunit/text-ui)

(define ltree
  (interior-node 'bar
    (leaf 26)
    (leaf 12)))

(define atree (interior-node  'bar ltree '()))

(define bintree-test
  (test-suite
    "Tests for binary tree"
    (check-equal? (leaf? '()) #f)
    (check-equal? (leaf? atree) #f)
    (check-equal? (contents-of atree) 'bar)
    (check-equal? (lson atree) ltree)
    (check-equal? (rson atree) '())
    (check-equal? (interior-node? atree) #t)
    (check-equal? (interior-node? '()) #f)
    (check-equal? (interior-node? (leaf 26)) #f)
    (check-equal? (contents-of (lson (lson atree))) 26)))

(run-tests bintree-test)

