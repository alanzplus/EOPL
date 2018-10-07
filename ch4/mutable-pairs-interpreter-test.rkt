#lang eopl

(require rackunit "mutable-pairs-interpreter.rkt")
(require rackunit/text-ui)

(define mutable-pairs-interpreter-test
  (test-suite
    "mutable-pairs-interpreter-test"
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
      (run "i")
      (num-val 1))
    (check-equal?
      (run "let f = proc(x) -(x,4) in (f 10)")
      (num-val 6))
    (check-equal?
      (run "letrec sum(x) = if zero?(x) then 0 else -((sum -(x,1)), 1) in (sum 5)")
      (num-val -5))
    (check-equal?
      (run "let x = 3 in let p = pair(11,x) in left(p)")
      (num-val 11))
    (check-equal?
      (run "let x = 3 in let p = pair(11,x) in right(p)")
      (num-val 3))
    (check-equal?
      (run "let p = pair(11,12) in setleft(p, 100)")
      (num-val 100))
    (check-equal?
      (run "let p = pair(11,12) in setright(p, 100)")
      (num-val 100))
    (check-equal?
      (run "let p = pair(11,12) in let t = setleft(p, 100) in left(p)")
      (num-val 100))
    (check-equal?
      (run "let p = pair(11,12) in let t = setleft(p, 100) in right(p)")
      (num-val 12))
    (check-equal?
      (run "let p = pair(11,12) in let t = setright(p, 100) in right(p)")
      (num-val 100))
    (check-equal?
      (run "let p = pair(11,12) in let t = setright(p, 100) in left(p)")
      (num-val 11))
  ))

(run-tests mutable-pairs-interpreter-test)
