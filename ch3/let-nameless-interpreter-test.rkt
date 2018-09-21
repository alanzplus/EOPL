#lang eopl

(require rackunit "let-nameless-interpreter.rkt")
(require rackunit/text-ui)

(define let-test
  (test-suite
    "Tests for LET Nameless interpreter"
    (check-equal?
      (run "-(3,4)")
      (num-val -1))
    (check-equal?
      (run "let fn = proc (x) x in -((fn 3),2)")
      (num-val 1))
    ))

(run-tests let-test)
