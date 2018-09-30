#lang eopl

(require rackunit "let-nameless-interpreter.rkt")
(require rackunit/text-ui)

(define let-cond-test
  (test-suite
    "Tests for letproc"
    (check-equal?
      (run "cond
          zero?(1) ==> 1
          zero?(0) ==> 4
        end")
      (num-val 4))
  ))

(run-tests let-cond-test)
