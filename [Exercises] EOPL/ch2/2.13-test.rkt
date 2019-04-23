#lang eopl

(require rackunit "2.13.rkt")
(require rackunit/text-ui)

(define empty-env-test
  (test-suite
    "Tests for empty env"
    (check-equal? (empty-env? (empty-env)) #t)
    (check-equal? (empty-env? (extend-env 'a 1 (empty-env))) #f)))

(run-tests empty-env-test)
