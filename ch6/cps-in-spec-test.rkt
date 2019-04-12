#lang eopl

(require rackunit "cps-in-spec.rkt")
(require rackunit/text-ui)

(define cps-in-spec-test
  (test-suite
   "cps-in-spec-test"
   (check-equal?
    (scan-parse "a")
    (a-program (var-exp 'a)))
   (check-equal?
    (scan-parse "1984")
    (a-program (const-exp 1984)))
   (check-equal?
    (scan-parse "-(1,1)")
    (a-program (diff-exp (const-exp 1) (const-exp 1))))
   (check-equal?
    (scan-parse "zero?(-(1,1))")
    (a-program (zero?-exp (diff-exp (const-exp 1) (const-exp 1)))))
   (check-equal?
    (scan-parse "proc(a,b,c) -(a,b)")
    (a-program (proc-exp '(a b c) (diff-exp (var-exp 'a) (var-exp 'b)))))
   (check-equal?
    (scan-parse "let a = 10 in -(a,10)")
    (a-program (let-exp 'a (const-exp 10) (diff-exp (var-exp 'a) (const-exp 10)))))
   (check-equal?
    (scan-parse "
        letrec
            f1(a,b) = -(a,b)
            f2(a) = a
        in (f1 3 4)")
    (a-program (letrec-exp
                '(f1 f2)
                (list '(a b) '(a))
                (list
                 (diff-exp (var-exp 'a) (var-exp 'b))
                 (var-exp 'a))
                (call-exp (var-exp 'f1) (list (const-exp 3) (const-exp 4))))))))

(run-tests cps-in-spec-test)
