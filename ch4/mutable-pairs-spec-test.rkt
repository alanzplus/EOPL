#lang eopl

(require rackunit "mutable-pairs-spec.rkt")
(require rackunit/text-ui)

(define mutable-pairs-spec-test
  (test-suite
    "mutable-pairs-spec-test"
    (check-equal?
      (scan-parse "a")
      (a-program (var-exp 'a)))
    (check-equal?
      (scan-parse "3")
      (a-program (const-exp 3)))
    (check-equal?
      (scan-parse "-(a,3)")
      (a-program
        (diff-exp
          (var-exp 'a)
          (const-exp 3))))
    (check-equal?
      (scan-parse "zero?(3)")
      (a-program (zero?-exp (const-exp 3))))
    (check-equal?
      (scan-parse "if zero?(3) then 3 else 0")
      (a-program
        (if-exp (zero?-exp (const-exp 3)) (const-exp 3) (const-exp 0))))
    (check-equal?
      (scan-parse "let x = proc(x) x in (x 10)")
      (a-program
        (let-exp
          'x
          (proc-exp 'x (var-exp 'x))
          (call-exp (var-exp 'x) (const-exp 10)))))
    (check-equal?
      (scan-parse "letrec rec(x) = (rec x) in (rec 10)")
      (a-program
        (letrec-exp
          'rec
          'x
          (call-exp (var-exp 'rec) (var-exp 'x))
          (call-exp (var-exp 'rec) (const-exp 10)))))
    (check-equal?
      (scan-parse "set x = 3")
      (a-program
        (assign-exp 'x (const-exp 3))))
    (check-equal?
      (scan-parse "pair(1,3)")
      (a-program
        (pair-exp (const-exp 1) (const-exp 3))))
    (check-equal?
      (scan-parse "left(loc)")
      (a-program
        (left-exp (var-exp 'loc))))
    (check-equal?
      (scan-parse "right(loc)")
      (a-program
        (right-exp (var-exp 'loc))))
    (check-equal?
      (scan-parse "setleft(loc,3)")
      (a-program
        (setleft-exp (var-exp 'loc) (const-exp 3))))
    (check-equal?
      (scan-parse "setright(loc,3)")
      (a-program
        (setright-exp (var-exp 'loc) (const-exp 3))))
  ))

(run-tests mutable-pairs-spec-test)