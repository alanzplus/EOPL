#lang eopl

(require rackunit "A-spec.rkt")
(require rackunit/text-ui)

(define A-spec-test
  (test-suite
    "A-spec-test"
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
      (scan-parse "begin a end")
      (a-program
        (begin-exp (var-exp 'a) '())))
    (check-equal?
      (scan-parse
        "begin
          a;
          b
        end")
      (a-program (begin-exp (var-exp 'a) (list (var-exp 'b)))))
  ))

(run-tests A-spec-test)
