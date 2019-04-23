#lang eopl

(require rackunit "explicit-refs-interpreter.rkt")
(require rackunit/text-ui)

(define list-test
  (test-suite
    "Test for list"
    (check-equal?
      (run "let x = 3 in list(1,-(2,3),x)")
      (list-val (list (num-val 1) (num-val -1) (num-val 3))))
  ))

(run-tests list-test)
