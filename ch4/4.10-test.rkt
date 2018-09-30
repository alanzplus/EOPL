#lang eopl

(require rackunit "explicit-refs-interpreter.rkt")
(require rackunit/text-ui)

(define begin-test
  (test-suite
    "Test for begin"
    (check-equal?
      (run "
        let x = newref(1) in
        begin
          setref(x, 11);
          setref(x, 12);
          deref(x)
        end")
      (num-val 12))
  ))

(run-tests begin-test)
