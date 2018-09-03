#lang eopl

(require rackunit "exe1.20.rkt")
(require rackunit/text-ui)

(define count-occurrences-test
  (test-suite
    "Tests for (count-occurrences s slist)"
    (check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
    (check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
    (check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)))

(run-tests count-occurrences-test)
