#lang eopl

(require rackunit "implicit-refs-spec.rkt")
(require rackunit/text-ui)


(define explicit-refs-spec-test
  (test-suite
    "Tests for explicit reference spec"
    (check-equal?
      (scan-parse "set a = 3")
      (a-program (assign-exp 'a (const-exp 3))))
  ))

(run-tests explicit-refs-spec-test)
