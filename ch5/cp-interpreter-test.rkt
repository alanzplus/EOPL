#lang eopl

(require rackunit "cp-interpreter.rkt")
(require rackunit/text-ui)

(define cp-interpreter-test
  (test-suite
    "cp-interpreter-test"
    (check-equal?
      (run "3")
      (num-val 3))
    (check-equal?
      (run "let x = 0 in if zero?(x) then 3 else 4")
      (num-val 3))
    (check-equal?
      (run "-(3,4)")
      (num-val -1))
    (check-equal?
      (run "let x = 3 in -(1,x)")
      (num-val -2))
    (check-equal?
      (run "let f = proc(x) -(x,4) in (f 10)")
      (num-val 6))
    (check-equal?
      (run "let f = proc(x y z) -(-(x,y),z) in (f 10 9 5)")
      (num-val -4))
    (check-equal?
      (run "letrec sum(x) = if zero?(x) then 0 else -((sum -(x,1)), 1) in (sum 5)")
      (num-val -5))
    (check-equal?
      (run "letrec sum(x y) = if zero?(x) then y else (sum -(x,1) -(1,-(0,y))) in (sum 5 1)")
      (num-val 6))
    ; (check-equal?
    ;   (run "let x = 3 in begin x; 4; x end")
    ;   (num-val 3))
  ))

(run-tests cp-interpreter-test)
