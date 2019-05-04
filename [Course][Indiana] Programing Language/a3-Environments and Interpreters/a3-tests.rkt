#lang racket

(require rackunit "a3.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A3:"
              (test-suite "Part 1: Interpreters and Environments"
                          (test-equal? "case1"
                                      (value-of
                                        '((lambda (x) (if (zero? x)
                                                        #t
                                                        #f))
                                          0)
                                        (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                      #t)
                          (test-equal? "case2"
                                       (value-of 
                                         '((lambda (x) (if (zero? x) 
                                                         12 
                                                         47)) 
                                           0) 
                                         (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       12)
                          (test-equal? "case3" (value-of
                                                 '(let ([y (* 3 4)])
                                                    ((lambda (x) (* x y)) (sub1 6)))
                                                 (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       60)
                          (test-equal? "case4" (value-of
                                                 '(let ([x (* 2 3)])
                                                    (let ([y (sub1 x)])
                                                      (* x y)))
                                                 (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       30)
                          (test-equal? "case5"
                                       (value-of
                                         '(let ([x (* 2 3)])
                                            (let ([x (sub1 x)])
                                              (* x x)))
                                         (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       25)
                          (test-equal? "case6" (value-of 
                                                 '(let ((! (lambda (x) (* x x))))
                                                    (let ((! (lambda (n)
                                                               (if (zero? n) 1 (* n (! (sub1 n)))))))
                                                      (! 5)))
                                                 (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       80)
                          (test-equal? "case7" (value-of
                                                 '(((lambda (f)
                                                      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
                                                    (lambda (f)
                                                      (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
                                                   5)
                                                 (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       120)
                          (test-equal? "case8" (value-of-fn
                                                 '((lambda (x) (if (zero? x)
                                                                 #t
                                                                 #f))
                                                   0)
                                                 (empty-env-fn))
                                       #t)
                          (test-equal? "case9" (value-of-fn 
                                         '((lambda (x) (if (zero? x) 
                                                         12 
                                                         47)) 
                                           0) 
                                         (empty-env-fn))
                                       12)
                          (test-equal? "case10" (value-of-fn
                                         '(let ([y (* 3 4)])
                                            ((lambda (x) (* x y)) (sub1 6)))
                                         (empty-env-fn))
                                       60)
                          (test-equal? "case11" (value-of-fn
                                         '(let ([x (* 2 3)])
                                            (let ([y (sub1 x)])
                                              (* x y)))
                                         (empty-env-fn))
                                       30)
                          (test-equal? "case12" (value-of-fn
                                         '(let ([x (* 2 3)])
                                            (let ([x (sub1 x)])
                                              (* x x)))
                                         (empty-env-fn))
                                       25)
                          (test-equal? "case13" (value-of-fn
                                                  '(((lambda (f)
                                                       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
                                                     (lambda (f)
                                                       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
                                                    5)
                                                  (empty-env-fn))
                                       120)
                          (test-equal? "case14" (value-of-ds
                                                  '((lambda (x) (if (zero? x)
                                                                  #t
                                                                  #f))
                                                    0)
                                                  (empty-env-ds))
                                       #t) 
                          (test-equal? "case15" (value-of-ds
                                                  '((lambda (x) (if (zero? x) 
                                                                  12 
                                                                  47)) 
                                                    0) 
                                                  (empty-env-ds))
                                       12)
                          (test-equal? "case16" (value-of-ds
                                                  '(let ([y (* 3 4)])
                                                     ((lambda (x) (* x y)) (sub1 6)))
                                                  (empty-env-ds))
                                       60)
                          (test-equal? "case17" (value-of-ds
                                                  '(let ([x (* 2 3)])
                                                     (let ([y (sub1 x)])
                                                       (* x y)))
                                                  (empty-env-ds))
                                       30)
                          (test-equal? "case18" (value-of-ds
                                                  '(let ([x (* 2 3)])
                                                     (let ([x (sub1 x)])
                                                       (* x x)))
                                                  (empty-env-ds))
                                       25)
                          (test-equal? "case19" (value-of-ds
                                                  '(((lambda (f)
                                                       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n))))))
                                                     (lambda (f)
                                                       (lambda (n) (if (zero? n) 1 (* n ((f f) (sub1 n)))))))
                                                    5)
                                                  (empty-env-ds))
                                       120)

                          (test-equal? "case20" (fo-eulav '(5 (x (x) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y))) 5)
                          (test-equal? "case21" (fo-eulav '(((x 1bus) (x) adbmal) ((5 f) (f) adbmal)) (lambda (y) (error 'fo-eulav "unbound variable ~s" y))) 4)
                          (test-equal? "case22" (fo-eulav   '(5
                                                              (((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
                                                                 (n) adbmal)
                                                                (f) adbmal)
                                                               ((((((n 1bus) (f f)) n *) 1 (n ?orez) fi)
                                                                 (n) adbmal)
                                                                (f) adbmal))) 
                                                            (lambda (y) (error 'fo-eulav "unbound variable ~s" y)))
                                       120)
                          (test-equal? "case23" (value-of
                                                  '(* (begin2 1 1) 3)
                                                  (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       3)
                          (test-equal? "case24" (value-of
                                                  '((lambda (a)
                                                      ((lambda (p)
                                                         (begin2
                                                           (p a)
                                                           a))
                                                       (lambda (x) (set! x 4))))
                                                    3)
                                                  (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       3)
                          (test-equal? "case25" (value-of
                                                  '((lambda (f)
                                                      ((lambda (g)
                                                         ((lambda (z) (begin2
                                                                        (g z)
                                                                        z))
                                                          55))
                                                       (lambda (y) (f y)))) (lambda (x) (set! x 44)))
                                                  (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       55)
                          (test-equal? "case26"  (value-of
                                                   '((lambda (x)
                                                       (begin2 (set! x 5) x))
                                                     6)
                                                   (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       5)
                          (test-equal? "case27" (value-of 
                                                  '(let ((a 3)) 
                                                     (begin2 (begin2 a (set! a 4)) a))
                                                  (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       4)
                          (test-equal? "case28" (value-of 
                                                  '((lambda (x)
                                                      (begin2
                                                        ((lambda (y)
                                                           (begin2
                                                             (set! x 0)
                                                             98))
                                                         99)
                                                        x))
                                                    97)
                                                  (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       0)
                          (test-equal? "case29" (value-of 
                                                  '((lambda (y)
                                                      (let ((x (begin2
                                                                 (set! y 7)
                                                                 8)))
                                                        (begin2
                                                          (set! y 3)
                                                          ((lambda (z) y)
                                                           x))))
                                                    4)
                                                  (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       3)
                          (test-equal? "case30" (value-of 
                                                  '(let ((a 5))
                                                     (let ((y (begin2 (set! a (sub1 a)) 6)))
                                                       (begin2
                                                         (* y y)
                                                         a)))
                                                  (lambda (y) (error 'value-of "unbound variable ~s" y)))
                                       4)
)))

(run-tests tests)
