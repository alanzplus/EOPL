#lang eopl

(require rackunit "minimal-template.rkt")
(require rackunit/text-ui)


(define minimal-interpereter-test
  (test-suite
    "Tests for minimal interpreter test"
    (check-equal?
      (run "3")
      (num-val 3))
    (check-equal?
      (run "if zero?(0) then -(1,3) else -(1,4)")
      (num-val -2))
    (check-equal?
      (run "let x = 3 in -(1,x)")
      (num-val -2))
    (check-equal?
      (run "let f = proc (x) -(x,4) in (f 10)")
      (num-val 6))
  ))

(run-tests minimal-interpereter-test)
