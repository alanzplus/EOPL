#lang eopl

(require rackunit "2.5.rkt")
(require rackunit/text-ui)

(define test-env
  (extend-env
    'a 10
    (extend-env
      'b 11
      (extend-env
        'c 12
        (empty-env)))))

(define env-test
  (test-suite
    "Tests for env"
    (check-equal? (empty-env) '())
    (check-equal? (empty-env? (empty-env)) #t)
    (check-equal?
      (extend-env 'a 10 (empty-env))
      (cons (list 'a 10) (empty-env)))
    (check-equal?
      (apply-env (extend-env 'a 10 empty-env) 'a)
      10)
    (check-equal?
      (extend-env* '(a b c) '(10 11 12) (empty-env))
      test-env)
    (check-equal?
      (has-binding? test-env 'a) #t)
    (check-equal?
      (has-binding? test-env 'k) #f)))

(run-tests env-test)