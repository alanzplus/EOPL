#lang eopl

(require rackunit "2.18.rkt")
(require rackunit/text-ui)

(define test-lst
  '(6 (5 4 3 2 1) (7 8 9)))

(define node-in-sequence-test
  (test-suite
    "Tests for node in sequence"
    (check-equal? (number->sequence 7) '(7 () ()))
    (check-equal? (current-element test-lst) 6)
    (check-equal? (move-to-left test-lst) '(5 (4 3 2 1) (6 7 8 9)))
    (check-equal? (move-to-right test-lst) '(7 (6 5 4 3 2 1) (8 9)))
    (check-equal? (insert-to-left 13 test-lst) '(6 (13 5 4 3 2 1) (7 8 9)))
    (check-equal? (insert-to-right 13 test-lst) '(6 (5 4 3 2 1) (13 7 8 9)))
    ))

(run-tests node-in-sequence-test)
