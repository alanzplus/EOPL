#lang eopl

(require rackunit "2.22.rkt")
(require rackunit/text-ui)

(define stack-test
  (test-suite
    "Tests for stack"
    (check-equal? (top (push 1 (empty-stack))) 1)
    (check-equal? (top (pop (push 2 (push 1 (empty-stack))))) 1)
    (check-equal? (empty-stack? (push 1 (empty-stack))) #f)
    (check-equal? (empty-stack? (empty-stack)) #t)
    ))

(run-tests stack-test)
