#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "letmul a = 1 b = 2 in -(a,b)")
      (num-val -1))
    (check-equal?
      (run "let x = 30 in letmul x = -(x,1) y = -(x,2) in -(x,y)")
      (num-val 1))
))

(run-tests cp-interpreter-test)