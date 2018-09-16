#lang eopl

(require rackunit "2.14.rkt")
(require rackunit/text-ui)

(define has-binding-test
  (test-suite
    "Tests for has binding"
    (check-equal? (has-binding? 'a (empty-env)) #f)
    (check-equal? (has-binding? 'a (extend-env 'a 3 (empty-env))) #t)))

(run-tests has-binding-test)
