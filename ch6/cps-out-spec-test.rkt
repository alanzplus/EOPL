#lang eopl

(require rackunit "cps-out-spec.rkt")
(require rackunit/text-ui)

(define cps-out-spec-test
  (test-suite
   "cps-spec-test"
   (check-equal?
    (cps-scan-parse "a")
    (cps-a-program (simple-exp->exp (cps-var-exp 'a))))
   (check-equal?
    (cps-scan-parse "1984")
    (cps-a-program (simple-exp->exp (cps-const-exp 1984))))
   (check-equal?
    (cps-scan-parse "-(1,1)")
    (cps-a-program (simple-exp->exp (cps-diff-exp (cps-const-exp 1) (cps-const-exp 1)))))
   (check-equal?
    (cps-scan-parse "zero?(-(1,1))")
    (cps-a-program (simple-exp->exp (cps-zero?-exp (cps-diff-exp (cps-const-exp 1) (cps-const-exp 1))))))
   (check-equal?
    (cps-scan-parse "proc(a,b,c) -(a,b)")
    (cps-a-program (simple-exp->exp (cps-proc-exp '(a b c) (simple-exp->exp (cps-diff-exp (cps-var-exp 'a) (cps-var-exp 'b)))))))
   (check-equal?
    (cps-scan-parse "let a = 10 in -(a,10)")
    (cps-a-program (cps-let-exp 'a (cps-const-exp 10) (simple-exp->exp (cps-diff-exp (cps-var-exp 'a) (cps-const-exp 10))))))
   (check-equal?
    (cps-scan-parse "
        letrec
            f1(a,b) = -(a,b)
            f2(a) = a
        in (f1 3 4)")
    (cps-a-program (cps-letrec-exp
                '(f1 f2)
                (list '(a b) '(a))
                (list
                 (simple-exp->exp (cps-diff-exp (cps-var-exp 'a) (cps-var-exp 'b)))
                 (simple-exp->exp (cps-var-exp 'a)))
                (cps-call-exp (cps-var-exp 'f1) (list (cps-const-exp 3) (cps-const-exp 4))))))))

(run-tests cps-out-spec-test)
