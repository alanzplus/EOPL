#lang racket

(require rackunit "a6.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A6:"
              (test-suite "binary-to-decimal"
                          (test-equal? "case1"
                                       (binary-to-decimal '()) 0)
                          (test-equal? "case2"
                                       (binary-to-decimal '(1)) 1)
                          (test-equal? "case3"
                                       (binary-to-decimal '(0 1)) 2)
                          (test-equal? "case4"
                                       (binary-to-decimal '(1 1 0 1)) 11)
                          (test-equal? "case1"
                                       (binary-to-decimal-cps '() (empty-k)) 0)
                          (test-equal? "case2"
                                       (binary-to-decimal-cps '(1) (empty-k)) 1)
                          (test-equal? "case3"
                                       (binary-to-decimal-cps '(0 1) (empty-k)) 2)
                          (test-equal? "case4"
                                       (binary-to-decimal-cps '(1 1 0 1) (empty-k)) 11)
                         ) 

))

(run-tests tests)
