#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define letproc-test
  (test-suite
    "Tests for letproc"
    (check-equal?
      (run "
      letrec
        even(x dummy) = if zero?(x) then 1 else (odd -(x,1) dummy)
        odd(x dummy) = if zero?(x) then 0 else (even -(x,1) dummy)
      in (odd 13 12)")
      (num-val 1))
  ))

(run-tests letproc-test)
