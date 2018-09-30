#lang eopl

(require rackunit "explicit-refs-interpreter.rkt")
(require rackunit/text-ui)

(define explicit-refs-interpreter-test
  (test-suite
    "Tests for explicit reference interpreter"
    (check-equal?
      (run "newref(1)")
      (ref-val 0))
    (check-equal?
      (run "let a = newref(1) in deref(a)")
      (num-val 1))
    (check-equal?
      (run "let a = newref(1) in
              let b = setref(a, 10) in deref(a)")
      (num-val 10))
  ))

(run-tests explicit-refs-interpreter-test)
