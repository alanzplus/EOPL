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
                          )
))

(run-tests tests)
