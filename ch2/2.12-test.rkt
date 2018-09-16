#lang eopl

(require rackunit "2.12.rkt")
(require rackunit/text-ui)

(define astack
  (push
    1
    (push 2 (empty-stack))))

(define stack-tests
  (test-suite
    "Tests for stack"
    (check-equal? (top astack) 1)
    (check-equal? (top (pop astack)) 2)))

(run-tests stack-tests)
