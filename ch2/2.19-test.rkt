#lang eopl

(require rackunit "2.19.rkt")
(require rackunit/text-ui)

(define simple-tree
  (number->bintree 13))

(define bintree-test
  (test-suite
    "Tests for bintree"
    (check-equal? simple-tree '(13 () ()))
    (check-equal? (at-leaf? simple-tree) #t)
    (check-equal?
      (insert-to-left 14 simple-tree)
      '(13 (14) ()))
    (check-equal?
      (insert-to-left 15 (insert-to-left 14 simple-tree))
      '(13 (15 (14) ()) ()))
    (check-equal?
      (insert-to-right 15 (insert-to-right 14 simple-tree))
      '(13 () (15 (14) ())))))

(run-tests bintree-test)
