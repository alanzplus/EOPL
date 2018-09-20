#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define letproc-test
  (test-suite
    "Tests for letproc"
    (check-equal?
      (run "letproc fn = (x) -(x,1) in (fn 3)")
      (num-val 2))
  ))

(run-tests letproc-test)
