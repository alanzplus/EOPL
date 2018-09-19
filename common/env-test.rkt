#lang eopl

(require rackunit "env.rkt")
(require rackunit/text-ui)

(define env-test
  (test-suite
    "Tests for env"
    (check-equal?
      (apply-env (extend-env 'a 3 (empty-env)) 'a)
      3)
  ))

(run-tests env-test)
