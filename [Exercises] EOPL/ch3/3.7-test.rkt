#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define let-test
  (test-suite
    "Tests for let"
    (check-equal?
      (run "+(3, +(1,4))")
      (num-val 8))
    (check-equal?
      (run "*(3, *(1,4))")
      (num-val 12))
    (check-equal?
      (run "/(3,4)")
      (num-val 0))
    (check-equal?
      (run "/(4,3)")
      (num-val 1))
  ))

(run-tests let-test)
