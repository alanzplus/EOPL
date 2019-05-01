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
    (test-suite "extend"
                (test-equal? "case1" ((extend 1 even?) 0) #t)
                (test-equal? "case2" ((extend 1 even?) 1) #t)
                (test-equal? "case3" ((extend 1 even?) 2) #t)
                (test-equal? "case4" ((extend 1 even?) 3) #f)
                (test-equal? "case5" (filter (extend 1 even?) '(0 1 2 3 4 5)) '(0 1 2 4))
                (test-equal? "case6" (filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5)) '(0 1 2 3 4))
                (test-equal? "case7" (filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5)) '(0 1 2 3 4)))
))

(run-tests tests)
