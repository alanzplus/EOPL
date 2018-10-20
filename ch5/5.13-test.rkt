#lang eopl

(require rackunit "cp-interpreter.rkt")

(eopl:pretty-print "--------------- fact ---------------")
(run "
  letrec fact(n) =
      if zero?(n) then
        1
      else
        *(n,(fact -(n,1)))
    in (fact 4)")

(eopl:pretty-print "--------------- fact iter ---------------")
(run "
  letrec fact(n res) =
      if zero?(n) then
        res
      else
        (fact -(n,1) *(n,res))
   in (fact 4 1)")