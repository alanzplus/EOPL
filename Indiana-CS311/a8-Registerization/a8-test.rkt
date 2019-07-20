#lang racket

(require rackunit "a8.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A8:"
              (test-suite ""
                          (test-equal "case1"
                                      (lambda () ) '()))))

(run-tests tests)
