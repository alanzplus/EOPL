#lang eopl
(require rackunit "list-length.rkt")
(require rackunit/text-ui)

(define list-length-test
  (test-suite
    "Tests for list-length.rkt"
    (check-equal? (list-length '()) 0 "Empty list")
    (check-equal? (list-length '(a b)) 2 "Simple list")
    (check-equal? (list-length '(a b (a b c))) 3 "Nested list")))

(run-tests list-length-test) 
