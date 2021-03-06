#lang racket

(require rackunit "a5.rkt")
(require rackunit/text-ui)

(define random-sieve
  '((lambda (n)
      (if (zero? n)
        (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) (if (zero? n) #t #f) #f) #f) #f) #f) #f)
        (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f (if (zero? n) #f #t))))))))
    (random 2)))

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
              (test-suite "call-by-value"
                          (test-equal? "case1" (val-of-cbv
                                                 '((lambda (a)
                                                     ((lambda (p)
                                                        (begin2
                                                          (p a)
                                                          a)) (lambda (x) (set! x 4)))) 3)
                                                 (empty-env))
                                       3)
                          (test-equal? "case2" (val-of-cbv
                                                 '((lambda (f)
                                                     ((lambda (g)
                                                        ((lambda (z) (begin2
                                                                       (g z)
                                                                       z))
                                                         55))
                                                      (lambda (y) (f y)))) (lambda (x) (set! x 44)))
                                                 (empty-env))
                                       55)
                          (test-equal? "case3" (val-of-cbv
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
                                       33))
              (test-suite "call-by-name"
                          ;; call-by-name
                          ;;P(false positive) <= .01
                          (test-equal? "case1" (val-of-cbname random-sieve (empty-env))
                                       #f)
                          (test-equal? "case2" (val-of-cbname
                                                 '((lambda (z) 100)
                                                   ((lambda (x) (x x)) (lambda (x) (x x))))
                                                 (empty-env))
                                       100))
              (test-suite "call-by-need"
                          (test-equal? "case1" (val-of-cbneed random-sieve (empty-env)) #t))
))

(run-tests tests)
