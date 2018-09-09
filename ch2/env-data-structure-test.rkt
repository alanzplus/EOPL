#lang eopl

(require rackunit "env-data-structure.rkt")
(require rackunit/text-ui)

(define env-test
  (test-suite
    "Tests for env"
    (check-equal? (empty-env) (list 'empty-env))
    (check-equal?
      (extend-env 'a 10 empty-env)
      (list 'extend-env 'a 10 empty-env))
    (check-equal?
      (apply-env (extend-env 'a 10 empty-env) 'a)
      10)))

(run-tests env-test)
