#lang racket

(require rackunit "a7.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A7:"
              (test-suite "last-non-zero"
                          (test-equal? "case1"
                                       (last-non-zero '()) '())
                          (test-equal? "case2"
                                       (last-non-zero '(1 2 3 4 5)) '(1 2 3 4 5))
                          (test-equal? "case3"
                                       (last-non-zero '(0)) '())
                          (test-equal? "case4"
                                       (last-non-zero '(0 1)) '(1))
                          (test-equal? "case5"
                                       (last-non-zero '(1 0 1 2 3 4)) '(1 2 3 4))
                          (test-equal? "case6"
                                       (last-non-zero '(1 0 2 0 3 0 4)) '(4))
                          )
              )
  )

(run-tests tests)
