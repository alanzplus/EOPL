#lang eopl

(require rackunit "2.21.rkt")
(require rackunit/text-ui)

(define a-env
  (extend-env 'a 1 (empty-env)))

(define env-test
  (test-suite
    "Tests for env"
    (check-equal? (apply-env 'a a-env) 1)
    (check-equal? (has-binding? 'a a-env) #t)
    (check-equal? (has-binding? 'b a-env) #f)))

(run-tests env-test)
