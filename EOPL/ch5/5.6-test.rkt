#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "list()")
      (null-val))
    (check-equal?
      (run "list(1 2)")
      (pair-val (num-val 1) (pair-val (num-val 2) (null-val))))
    (check-equal?
      (run "list(1 list(2 3))")
      (pair-val
        (num-val 1)
        (pair-val
          (pair-val
            (num-val 2)
            (pair-val
              (num-val 3)
              (null-val)))
          (null-val))))
))

(run-tests cp-interpreter-test)