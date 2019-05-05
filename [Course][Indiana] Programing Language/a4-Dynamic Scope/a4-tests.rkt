#lang racket

(require rackunit "a4.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A4:"
              (test-suite "lex"
                          (test-equal? "case1" (lex '((lambda (x) x) 5)  '()) '((lambda (var 0)) (const 5)))
                          (test-equal? "case2"
                                      (lex '(lambda (!)
                                              (lambda (n)
                                                (if (zero? n) 1 (* n (! (sub1 n))))))
                                           '())
                                      '(lambda
                                         (lambda
                                           (if (zero? (var 0))
                                             (const 1)
                                             (* (var 0) ((var 1) (sub1 (var 0))))))))
                          (test-equal? "case3"
                                      (lex '(let ((! (lambda (!)
                                                       (lambda (n)
                                                         (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
                                              ((! !) 5))
                                           '())
                                      '(let (lambda
                                              (lambda
                                                (if (zero? (var 0))
                                                  (const 1)
                                                  (* (var 0) (((var 1) (var 1)) (sub1 (var 0)))))))
                                         (((var 0) (var 0)) (const 5)))))

              (test-suite "value-of"
                          (test-equal? "case1" (value-of-fn 
                                                 '((lambda (x) (if (zero? x) 
                                                                 12 
                                                                 47)) 
                                                   0) 
                                                 (empty-env))
                                       12)
                          (test-equal? "case2"
                                       (value-of-fn
                                         '(let ([y (* 3 4)])
                                            ((lambda (x) (* x y)) (sub1 6)))
                                         (empty-env))
                                       60)
                          (test-equal? "case3"
                                       (value-of-fn
                                         '(let ([x (* 2 3)])
                                            (let ([y (sub1 x)])
                                              (* x y)))
                                         (empty-env))
                                       30)
                          (test-equal? "case4"
                                       (value-of-fn
                                         '(let ([x (* 2 3)])
                                            (let ([x (sub1 x)])
                                              (* x x)))
                                         (empty-env))
                                       25)
                          (test-equal? "case5"
                                       (value-of-ds
                                         '((lambda (x) (if (zero? x) 
                                                         12 
                                                         47)) 
                                           0) 
                                         (empty-env))
                                       12)
                          (test-equal? "case6" (value-of-ds
                                                 '(let ([y (* 3 4)])
                                                    ((lambda (x) (* x y)) (sub1 6)))
                                                 (empty-env))
                                       60)
                          (test-equal? "case7" (value-of-ds
                                                 '(let ([x (* 2 3)])
                                                    (let ([y (sub1 x)])
                                                      (* x y)))
                                                 (empty-env))
                                       30)
                          (test-equal? "case8" (value-of-ds
                                                 '(let ([x (* 2 3)])
                                                    (let ([x (sub1 x)])
                                                      (* x x)))
                                                 (empty-env))
                                       25))

))

(run-tests tests)
