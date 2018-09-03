#lang eopl

(require rackunit "exe1.22.rkt")
(require rackunit/text-ui)

(define filter-in-test
  (test-suite
    "Tests for (filter-in pred lst)"
    (check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
    (check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))))

(run-tests filter-in-test)
