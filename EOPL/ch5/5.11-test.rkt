#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
        (run "letmul a = 3 b = 4 in begin set a = 10; set b = 11; b end")
        (num-val 11))
))

(run-tests cp-interpreter-test)