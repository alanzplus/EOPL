#lang eopl

(require rackunit "occurs-free.rkt")
(require rackunit/text-ui)

(define occurs-free-test
  (test-suite
    "Tests for occurs-free.rkt"
    (check-equal? (occurs-free? 'x 'x) #t)
    (check-equal? (occurs-free? 'x 'y) #f)
    (check-equal? (occurs-free? 'x '(lambda (x) (x y))) #f "(lambda (x) (x y))")
    (check-equal? (occurs-free? 'x '(lambda (y) (x y))) #t)
    (check-equal? (occurs-free? 'x '((lambda (x) x) (x y))) #t)
    (check-equal?
      (occurs-free? 'x
                    '(lambda (y) (lambda (z) (x (y z)))))
      #t)))

(run-tests occurs-free-test)
