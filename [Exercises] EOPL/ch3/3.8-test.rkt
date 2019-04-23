#lang eopl

(require rackunit "let-interpreter.rkt")
(require rackunit/text-ui)

(define let-test
  (test-suite
    "Tests for let"
    (check-equal?
      (run "greater?(3,4)")
      (bool-val #f))
    (check-equal?
      (run "greater?(4,4)")
      (bool-val #f))
    (check-equal?
      (run "greater?(4,3)")
      (bool-val #t))
    (check-equal?
      (run "less?(4,3)")
      (bool-val #f))
    (check-equal?
      (run "less?(3,3)")
      (bool-val #f))
    (check-equal?
      (run "less?(3,4)")
      (bool-val #t))
    (check-equal?
      (run "equal?(3,3)")
      (bool-val #t))
    (check-equal?
      (run "equal?(3,4)")
      (bool-val #f))
    (check-equal?
      (run "equal?(zero?(1),4)")
      (bool-val #f))
  ))

(run-tests let-test)
