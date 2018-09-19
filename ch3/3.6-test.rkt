#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define let-test
  (test-suite
    "Tests for let"
    (check-equal?
      (run "minus(-(minus(5), 9))")
      (num-val 14))
  ))

(run-tests let-test)
