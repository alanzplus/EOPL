#lang racket

(require rackunit "a2.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A2:"
    (test-suite "list-ref" 
                (test-equal? "case1" (list-ref '(a b c) 2) 'c)
                (test-equal? "case2" (list-ref '(a b c) 0) 'a))
    (test-suite "union"
                (test-equal? "case1" (union '() '()) '())
                (test-equal? "case2" (union '(x) '()) '(x))
                (test-equal? "case3" (union '(x) '(x)) '(x))
                (test-equal? "case4" (union '(x y) '(x z)) '(z x y)))
))

(run-tests tests)
