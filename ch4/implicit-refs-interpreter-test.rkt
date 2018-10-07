#lang eopl

(require rackunit "implicit-refs-interpreter.rkt")
(require rackunit/text-ui)

(define implicit-refs-interpreter-test
  (test-suite
    "Tests for Implicit Reference interpreter"
    (check-equal?
      (run "3")
      (num-val 3))
    (check-equal?
      (run "if zero?(0) then -(1,3) else -(1,4)")
      (num-val -2))
    (check-equal?
      (run "let x = 3 in -(1,x)")
      (num-val -2))
    (check-equal?
      (run "i")
      (num-val 1))
    (check-equal?
      (run "let f = proc(x) -(x,4) in (f 10)")
      (num-val 6))
    (check-equal?
      (run "letrec sum(x) = if zero?(x) then 0 else -((sum -(x,1)), 1) in (sum 5)")
      (num-val -5))
  ))

(run-tests implicit-refs-interpreter-test)
