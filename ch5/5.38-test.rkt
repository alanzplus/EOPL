#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
        (run "/(3,0)")
        (string-val "division by zero"))
))

(run-tests cp-interpreter-test)