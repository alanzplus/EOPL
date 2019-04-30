#lang racket

(require rackunit "a1.rkt")
(require rackunit/text-ui)

(define tests
  (test-suite "A2:"
              (test-suite "test")
))

(run-tests tests)
