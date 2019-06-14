#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
        (run "let f = proc(x) x in (f 1 2 3)")
        (string-val "procedure call with wrong number of arguments"))
))

(run-tests cp-interpreter-test)