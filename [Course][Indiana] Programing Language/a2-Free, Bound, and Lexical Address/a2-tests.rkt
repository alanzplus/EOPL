#lang racket

(require rackunit "a2.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A2:"
    (test-suite "list-ref" 
                (test-equal? "case1" (list-ref '(a b c) 2) 'c)
                (test-equal? "case2" (list-ref '(a b c) 0) 'a))
))

(run-tests tests)
