#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "let3 a = 3, b = 4, c = 5 in -(c,-(a,b))")
      (num-val 6))
))

(run-tests cp-interpreter-test)