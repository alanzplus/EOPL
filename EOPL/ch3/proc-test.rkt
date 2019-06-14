#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define proc-test
  (test-suite
    "Tests for proc"
    (check-equal?
      (run "let fun = proc (a) -(a,1) in (fun 3)")
      (num-val 2))
  ))

(run-tests proc-test)
