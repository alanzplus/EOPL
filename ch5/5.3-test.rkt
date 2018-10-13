#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "let2 a = 3, b = 4 in -(a,b)")
      (num-val -1))
))

(run-tests cp-interpreter-test)