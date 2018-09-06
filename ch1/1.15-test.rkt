#lang eopl

(require rackunit "1.15.rkt")
(require rackunit/text-ui)

(define duple-test
  (test-suite
    "Tests for duple"
    (check-equal? (duple 2 3) (list 3 3))
    (check-equal? (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
    (check-equal? (duple 0 '(blah)) '())))

(run-tests duple-test)
