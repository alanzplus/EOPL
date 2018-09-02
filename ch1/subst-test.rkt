#lang eopl

(require rackunit "subst.rkt")
(require rackunit/text-ui)

(define subst-test
  (test-suite
    "Tests for subst.rkt"
    (check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))))

(run-tests subst-test)
