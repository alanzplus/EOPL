#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "list()")
      (list-val '()))
    (check-equal?
      (run "list(1 2)")
      (list-val (list (num-val 1) (num-val 2))))
    (check-equal?
      (run "list(1 list(2 3))")
      (list-val (list (num-val 1) (list-val (list (num-val 2) (num-val 3))))))
))

(run-tests cp-interpreter-test)