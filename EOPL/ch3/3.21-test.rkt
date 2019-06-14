#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define letproc-test
  (test-suite
    "Tests for letproc"
    (check-equal?
      (run "let x = proc (a b) +(a,b) in (x 1 2)")
      (num-val 3))
  ))

(run-tests letproc-test)
