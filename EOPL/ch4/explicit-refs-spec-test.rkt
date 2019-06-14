#lang eopl

(require rackunit "explicit-refs-spec.rkt")
(require rackunit/text-ui)


(define explicit-refs-spec-test
  (test-suite
    "Tests for explicit reference spec"
    (check-equal?
      (scan-parse "setref(1,2)")
      (a-program (setref-exp (const-exp 1) (const-exp 2))))
    (check-equal?
      (scan-parse "deref(1)")
      (a-program (deref-exp (const-exp 1))))
    (check-equal?
      (scan-parse "newref(1)")
      (a-program (newref-exp (const-exp 1))))
    (check-equal?
      (scan-parse
        "begin
          setref(1,2)
         end")
      (a-program (begin-exp (setref-exp (const-exp 1) (const-exp 2)) '())))
    (check-equal?
      (scan-parse
        "begin
          setref(1,2);
          setref(3,4)
        end")
     (a-program
      (begin-exp
        (setref-exp (const-exp 1) (const-exp 2))
        (list (setref-exp (const-exp 3) (const-exp 4))))))
  ))

(run-tests explicit-refs-spec-test)
