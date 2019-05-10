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
              (test-suite "remv-first-9*"
                          (test-equal? "case1"
                                       (remv-first-9* '((1 2 (3) 9))) '((1 2 (3))))
                          (test-equal? "case2"
                                       (remv-first-9* '(9 (9 (9 (9)))))
                                       '((9 (9 (9)))))
                          (test-equal? "case3"
                                       (remv-first-9* '(((((9) 9) 9) 9) 9))
                                       '((((() 9) 9) 9) 9))
                          (test-equal? "case4"
                                       (remv-first-9*-cps '((1 2 (3) 9)) (empty-k)) '((1 2 (3))))
                          (test-equal? "case5"
                                       (remv-first-9*-cps '(9 (9 (9 (9)))) (empty-k))
                                       '((9 (9 (9)))))
                          (test-equal? "case6"
                                       (remv-first-9*-cps '(((((9) 9) 9) 9) 9) (empty-k))
                                       '((((() 9) 9) 9) 9)))
              (test-suite "cons-cell-count"
                          (test-equal? "case1"
                                       (cons-cell-count '()) 0)
                          (test-equal? "case2"
                                       (cons-cell-count '(1)) 1)
                          (test-equal? "case3"
                                       (cons-cell-count '(1 2)) 2)
                          (test-equal? "case3"
                                       (cons-cell-count '(1 (3))) 3)
                          (test-equal? "case4"
                                       (cons-cell-count '((1 (2)) (3))) 6)
                          (test-equal? "case5"
                                       (cons-cell-count-cps '() (empty-k)) 0)
                          (test-equal? "case2"
                                       (cons-cell-count-cps '(1) (empty-k)) 1)
                          (test-equal? "case3"
                                       (cons-cell-count-cps '(1 2) (empty-k)) 2)
                          (test-equal? "case3"
                                       (cons-cell-count-cps '(1 (3)) (empty-k)) 3)
                          (test-equal? "case4"
                                       (cons-cell-count-cps '((1 (2)) (3)) (empty-k)) 6))
              (test-suite "find"
                          (test-equal? "case1" (find 5 '((5 . a) (6 . b) (7 . c))) 'a)
                          (test-equal? "case2" (find 7 '((5 . a) (6 . 5) (7 . 6))) 'a)
                          (test-equal? "case3" (find 5 '((5 . 6) (9 . 6) (2 . 9))) 6)
                          (test-equal? "case5" (find-cps 5 '((5 . a) (6 . b) (7 . c)) (empty-k)) 'a)
                          (test-equal? "case5" (find-cps 7 '((5 . a) (6 . 5) (7 . 6)) (empty-k)) 'a)
                          (test-equal? "case6" (find-cps 5 '((5 . 6) (9 . 6) (2 . 9)) (empty-k)) 6))
              (test-suite "ack"
                          (test-equal? "case1" (ack-cps 1 0 (empty-k)) (ack 1 0))
                          (test-equal? "case2" (ack-cps 0 1 (empty-k)) (ack 0 1))
                          (test-equal? "case3" (ack-cps 1 1 (empty-k)) (ack 1 1))
                          (test-equal? "case4" (ack-cps 2 2 (empty-k)) (ack 2 2)))
              (test-suite "fib"
                          (test-equal? "case1" (fib-cps 1 (empty-k)) (fib 1))
                          (test-equal? "case2" (fib-cps 2 (empty-k)) (fib 2))
                          (test-equal? "case3" (fib-cps 10 (empty-k)) (fib 10)))
              (test-suite "unfold"
                          (test-equal? "case1" (unfold-cps null? car cdr '() (empty-k)) (unfold null? car cdr '()))
                          (test-equal? "case2" (unfold-cps null? car cdr '(a) (empty-k)) (unfold null? car cdr '(a)))
                          (test-equal? "case1" (unfold-cps null? car cdr '(a b c d e) (empty-k)) (unfold null? car cdr '(a b c d e))))
))

(run-tests tests)
