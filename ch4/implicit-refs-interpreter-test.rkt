#lang eopl

(require rackunit "implicit-refs-interpreter.rkt")
(require rackunit/text-ui)

(define explicit-refs-interpreter-test
  (test-suite
    "Tests for explicit reference interpreter"
    (check-equal?
      (run "i")
      (num-val 1))
    (check-equal?
      (run "let k = set i = 10 in i")
      (num-val 10))
    (check-equal?
      (run "let p = proc (a) a in (p 10)")
      (num-val 10))
  ))

(run-tests explicit-refs-interpreter-test)
