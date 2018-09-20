#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define letrec-test
  (test-suite
    "Tests for letproc"
    (check-equal?
      (run "letrec fn(a b) = if zero?(a) then b else (fn -(a,1) +(b,1)) in (fn 10 0)")
      (num-val 10))
  ))

(run-tests letrec-test)
