#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
        (run "let a = set i = 3 in i")
        (num-val 3))
    (check-equal?
        (run "let a = 3 in a")
        (num-val 3))
    (check-equal?
        (run "let f = proc(x y) -(x,y) in (f 3 4)")
        (num-val -1))
))

(run-tests cp-interpreter-test)