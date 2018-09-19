#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define let-test
  (test-suite
    "Tests for LET interpreter"
    (check-equal?
      (run "-(3,4)")
      (num-val -1))
    ))

(run-tests let-test)
