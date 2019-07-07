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
              (test-suite "lex"
                          (test-equal? "case1"
                                       (lex '(let/cc k 5) '()) '(letcc (const 5)))
                          (test-equal? "case2"
                                       (lex '(let/cc k (k 5)) '())  '(letcc (app (var 0) (const 5))))
                          (test-equal? "case3"
                                       (lex '(let/cc k (throw k 5)) '()) '(letcc (throw (var 0) (const 5))))
                          )
              (test-suite "value-of"
                          (test-equal? "case1"
                                       (value-of (lex '(let/cc k 5) '()) (empty-env)) 5)
                          (test-equal? "case2"
                                       (value-of (lex '(let/cc k (k 5)) '()) (empty-env)) 5)
                          (test-equal? "case3"
                                       (value-of (lex '(let/cc k (throw k 5)) '()) (empty-env)) 5)
                          )
              (test-suite "value-of-cps"
                          (test-equal? "case1"
                                       (value-of-cps '(const 5) (empty-env) (empty-k)) 5)
                          (test-equal? "case2"
                                       (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
                          (test-equal? "case3"
                                       (value-of-cps '(sub1 (const 5)) (empty-env) (empty-k)) 4)
                          (test-equal? "case4"
                                       (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
                          (test-equal? "case5"
                                       (value-of-cps '(zero (const 5)) (empty-env) (empty-k)) #f)
                          (test-equal? "cass6"
                                       (value-of-cps '(zero (sub1 (const 6))) (empty-env) (empty-k)) #f)
                          )
              )
  )

(run-tests tests)
