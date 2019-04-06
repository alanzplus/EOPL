#lang eopl

(require rackunit "cps-out-spec.rkt")
(require rackunit/text-ui)

(define cps-out-spec-test
  (test-suite
   "cps-spec-test"
   (check-equal?
    (scan-parse "a")
    (a-program (simple-exp->exp (var-exp 'a))))
   (check-equal?
    (scan-parse "1984")
    (a-program (simple-exp->exp (const-exp 1984))))
   (check-equal?
    (scan-parse "-(1,1)")
    (a-program (simple-exp->exp (cps-diff-exp (const-exp 1) (const-exp 1)))))
   (check-equal?
    (scan-parse "zero?(-(1,1))")
    (a-program (simple-exp->exp (cps-zero?-exp (cps-diff-exp (const-exp 1) (const-exp 1))))))
   (check-equal?
    (scan-parse "proc(a,b,c) -(a,b)")
    (a-program (simple-exp->exp (cps-proc-exp '(a b c) (simple-exp->exp (cps-diff-exp (var-exp 'a) (var-exp 'b)))))))
   (check-equal?
    (scan-parse "let a = 10 in -(a,10)")
    (a-program (cps-let-exp 'a (const-exp 10) (simple-exp->exp (cps-diff-exp (var-exp 'a) (const-exp 10))))))
   (check-equal?
    (scan-parse "
        letrec
            f1(a,b) = -(a,b)
            f2(a) = a
        in (f1 3 4)")
    (a-program (cps-letrec-exp
                '(f1 f2)
                (list '(a b) '(a))
                (list
                 (simple-exp->exp (cps-diff-exp (var-exp 'a) (var-exp 'b)))
                 (simple-exp->exp (var-exp 'a)))
                (cps-call-exp (var-exp 'f1) (list (const-exp 3) (const-exp 4))))))))

(run-tests cps-out-spec-test)
