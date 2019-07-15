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
                          (test-equal? "case7"
                                       (value-of-cps '(if (zero (const 5)) (const 3) (mult (const 2) (const 2))) (empty-env) (empty-k)) 4)
                          (test-equal? "case8"
                                       (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
                          (test-equal? "case9"
                                       (value-of-cps '(letcc (const 5)) (empty-env) (empty-k)) 5)
                          (test-equal? "case10"
                                       (value-of-cps '(letcc (throw (var 0) (const 5))) (empty-env) (empty-k)) 5)
                          (test-equal? "case11"
                                       (value-of-cps '(letcc (throw (var 0) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
                          (test-equal? "case12"
                                       (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
                          (test-equal? "case13"
                                       (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
                          (test-equal? "case14"
                                       (value-of-cps '(letcc (throw (app (lambda (var 0)) (var 0)) (mult (const 5) (const 5)))) (empty-env) (empty-k)) 25)
                          (test-equal? "case15"
                                       (value-of-cps '(letcc (sub1 (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
                          (test-equal? "case16"
                                       (value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
                          (test-equal? "case17"
                                       (value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
                          (test-equal? "case18"
                                       (value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)
                          (test-equal? "case19"
                                       (value-of-cps '(app (lambda (const 5)) (const 6)) (empty-env) (empty-k)) 5)
                          (test-equal? "case20"
                                       (value-of-cps '(app (lambda (var 0)) (const 5)) (empty-env) (empty-k)) 5)
                          (test-equal? "case21"
                                       (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
                          (test-equal? "case22"
                                       (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
                          (test-equal? "case23"
                                       (value-of-cps '(app (lambda (if (zero (var 0)) (const 4) (const 5))) (const 3)) (empty-env) (empty-k)) 5)
                          (test-equal? "case24"
                                       (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
                          (test-equal? "case25"
                                       (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
                          (test-equal? "case26"
                                       (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                                                     (empty-env)
                                                     (empty-k))
                                       4)
                          (test-equal? "case27"
                                       (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                                                     (empty-env)
                                                     (empty-k))
                                       4)
                          (test-equal? "case28"
                                       (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                                           (lambda
                                                             (lambda 
                                                               (if (zero (var 0))  
                                                                   (const 1)
                                                                   (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                                                     (empty-env)
                                                     (empty-k))
                                       1)

                          )
                          (test-suite "brainteaser"
                                      (test-equal? "case1" (car$ trib$) 0)
                                      (test-equal? "case2" (car$ (cdr$ trib$)) 1)
                                      (test-equal? "case3" (car$ (cdr$ (cdr$ trib$))) 1)
                                      (test-equal? "case4" (car$ (cdr$ (cdr$ (cdr$ trib$)))) 2)
                                      (test-equal? "case5" (take$ 7 trib$) '(0 1 1 2 4 7 13))
                         )
              )
  )

(run-tests tests)
