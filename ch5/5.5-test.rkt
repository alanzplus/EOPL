#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "cons(1 cons(2 3))")
      (pair-val (num-val 1) (pair-val (num-val 2) (num-val 3))))
    (check-equal?
      (run "cons(1 cons(2 emptylist))")
      (pair-val (num-val 1) (pair-val (num-val 2) (null-val))))
    (check-equal?
      (run "car(cons(1 cons(2 3)))")
      (num-val 1))
    (check-equal?
      (run "cdr(cons(1 cons(2 3)))")
      (pair-val (num-val 2) (num-val 3)))
    (check-equal?
      (run "car(cdr(cons(1 cons(2 3))))")
      (num-val 2))
    (check-equal?
      (run "null?(emptylist)")
      (bool-val #t))
    (check-equal?
      (run "null?(cons(1 2))")
      (bool-val #f))
))

(run-tests cp-interpreter-test)