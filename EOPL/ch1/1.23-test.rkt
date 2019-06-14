#lang eopl

(require rackunit "1.23.rkt")
(require rackunit/text-ui)

(define list-index-test
  (test-suite
    "Tests for (list-index pred lst)"
    (check-equal? (list-index number? '(a 2 (1 3) b 7)) 1)
    (check-equal? (list-index symbol? '(a (b c) 17 foo)) 0)
    (check-equal? (list-index symbol? '(1 2 (a b) 3)) #f)))

(run-tests list-index-test)
