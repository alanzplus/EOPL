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
                          (test-equal? "case5"
                                       (binary-to-decimal-cps '() (empty-k)) 0)
                          (test-equal? "case6"
                                       (binary-to-decimal-cps '(1) (empty-k)) 1)
                          (test-equal? "case7"
                                       (binary-to-decimal-cps '(0 1) (empty-k)) 2)
                          (test-equal? "case8"
                                       (binary-to-decimal-cps '(1 1 0 1) (empty-k)) 11)) 
              (test-suite "times"
                          (test-equal? "case1"
                                       (times '(1 2 3 4 5)) 120)
                          (test-equal? "case2"
                                       (times '(1 2 3 0 3)) 0)
                          (test-equal? "case3"
                                       (times-cps '(1 2 3 4 5) (empty-k)) 120)
                          (test-equal? "case4"
                                       (times-cps '(1 2 3 0 3) (empty-k)) 0))
              (test-suite "times-cps-shortcut"
                          (test-equal? "case1"
                                       (times-cps-shortcut '(1 2 3 4 5) (empty-k)) 120)
                          (test-equal? "case2"
                                       (times-cps-shortcut '(1 2 3 0 3) (empty-k)) 0))
              (test-suite "plus"
                          (test-equal? "case1"
                                       ((plus 2) 3) 5)
                          (test-equal? "case2"
                                       ((plus ((plus 2) 3)) 5) 10)
                          (test-equal? "case3"
                                       ((plus-cps 2 (empty-k)) 3) 5)
                          (test-equal? "case4"
                                       ((plus-cps ((plus-cps 2 (empty-k)) 3) (empty-k)) 5) 10))
))

(run-tests tests)
