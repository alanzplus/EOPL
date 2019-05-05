#lang racket

(require rackunit "a5.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A5:"
              (test-suite "call-by-reference"
                          ;; Making sure set! works
                          (test-equal? "case1" (val-of-cbr
                                                 '((lambda (x) (begin2 (set! x #t)
                                                                       (if x 3 5))) #f)
                                                 (empty-env))
                                       3)
                          ;; Returns 4 under CBR...
                          (test-equal? "case2" (val-of-cbr
                                                 '((lambda (a)
                                                     ((lambda (p)
                                                        (begin2
                                                          (p a)
                                                          a)) (lambda (x) (set! x 4)))) 3)
                                                 (empty-env))
                                       4)
                          (test-equal? "case3" (val-of-cbr
                                                 '(let ([x 3])
                                                    (begin2
                                                      (set! x 4)
                                                      x))
                                                 (empty-env))
                                       4)
                          (test-equal? "case4" (val-of-cbr
                                                 '(let ([x 3])
                                                    (begin2
                                                      ((lambda (y)
                                                         (set! y 10))
                                                      x)
                                                      x))
                                                 (empty-env))
                                       10)
                          (test-equal? "case5"
                                (val-of-cbr
                                  '((lambda (f)
                                      ((lambda (g)
                                         ((lambda (z) (begin2
                                                        (g z)
                                                        z))
                                          55))
                                       (lambda (y) (f y)))) (lambda (x) (set! x 44)))
                                  (empty-env))
                                44)
                          (test-equal? "case6" (val-of-cbr
                                                 '((lambda (swap)
                                                     ((lambda (a)
                                                        ((lambda (b)
                                                           (begin2
                                                             ((swap a) b)
                                                             a)) 44)) 33))
                                                   (lambda (x)
                                                     (lambda (y)
                                                       ((lambda (temp)
                                                          (begin2
                                                            (set! x y)
                                                            (set! y temp))) x))))
                                                 (empty-env))
                                       44))
))

(run-tests tests)
