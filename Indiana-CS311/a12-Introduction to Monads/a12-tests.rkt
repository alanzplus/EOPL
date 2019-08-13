#lang racket

(require rackunit "a12.rkt")
(require rackunit/text-ui)


(define tests
  (test-suite "A12:"))

(run-tests tests)
