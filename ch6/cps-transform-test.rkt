#lang eopl

(require "../libs/common.rkt")
(require "cps-in-spec.rkt")
(require "cps-out-spec.rkt")
(require "cps-transform.rkt")
(require rackunit "cps-in-spec.rkt")
(require rackunit/text-ui)

(define cps-transform-test
  (test-suite
    "cps-transform-test"
    (check-equal?
      (cps-of-program (scan-parse "3"))
      (cps-a-program (simple-exp->exp (cps-const-exp 3))))
    (check-equal?
      (cps-of-program (scan-parse "a"))
      (cps-a-program (simple-exp->exp (cps-var-exp 'a))))
    (check-equal?
      (cps-of-program (scan-parse "-(3,a)"))
      (cps-a-program (simple-exp->exp (cps-diff-exp (cps-const-exp 3) (cps-var-exp 'a)))))
    (check-equal?
      (cps-of-program (scan-parse "proc (x) -(3,x)"))
      (cps-a-program (simple-exp->exp
                       (cps-proc-exp
                         '(x k%00)
                         (cps-call-exp (cps-var-exp 'k%00) (list (cps-diff-exp (cps-const-exp 3) (cps-var-exp 'x))))))))
    (check-equal?
      (cps-of-program (scan-parse "let x = 3 in -(x,3)"))
      (cps-a-program
        (cps-let-exp
          'x
          (cps-const-exp 3)
          (cps-call-exp
            (cps-proc-exp
              (list 'var%1) 
              (simple-exp->exp (cps-var-exp 'var%1)))
            (list
              (cps-diff-exp
                (cps-var-exp 'x)
                (cps-const-exp 3)))))))
    (check-equal?
      (cps-of-program (scan-parse "letrec fun (x) = x in 3"))
      (cps-a-program
        (cps-letrec-exp
          '(fun)
          (list (list 'x 'k%00))
          (list (cps-call-exp
                  (cps-var-exp 'k%00)
                  (list (cps-var-exp 'x))))
          (cps-call-exp
            (cps-proc-exp
              '(var%2)
              (simple-exp->exp (cps-var-exp 'var%2)))
            (list (cps-const-exp 3))))))
    (check-equal?
      (cps-of-program (scan-parse "if (f x) then (g y) else (h z)"))
      (cps-a-program
        (cps-call-exp
          (cps-var-exp 'f)
          (list (cps-var-exp 'x)
           (cps-proc-exp
             '(var%4)
             (cps-if-exp
               (cps-var-exp 'var%4)
               (cps-call-exp
                 (cps-var-exp 'g)
                 (list (cps-var-exp 'y)
                  (cps-proc-exp
                    '(var%3)
                    (simple-exp->exp (cps-var-exp 'var%3)))))
               (cps-call-exp
                 (cps-var-exp 'h)
                 (list (cps-var-exp 'z)
                  (cps-proc-exp
                    '(var%3)
                    (simple-exp->exp (cps-var-exp 'var%3)))))))))))
    (check-equal?
      (cps-of-program (scan-parse "(f (g x) (h (l y)))"))
      (cps-a-program
        (cps-call-exp
          (cps-var-exp 'g)
          (list (cps-var-exp 'x)
                (cps-proc-exp
                  '(var%6)
                  (cps-call-exp
                    (cps-var-exp 'l)
                    (list (cps-var-exp 'y)
                          (cps-proc-exp
                            '(var%8)
                            (cps-call-exp
                              (cps-var-exp 'h)
                              (list (cps-var-exp 'var%8)
                                    (cps-proc-exp
                                      '(var%7)
                                      (cps-call-exp
                                        (cps-var-exp 'f)
                                        (list
                                          (cps-var-exp 'var%6)
                                          (cps-var-exp 'var%7)
                                          (cps-proc-exp
                                            '(var%5)
                                            (simple-exp->exp
                                              (cps-var-exp 'var%5))))))))))))))))
    (check-equal?
      (cps-of-program (scan-parse "let x = 3 in letrec fun (k) = if zero?(-(k,x)) then (fun 1) else -(1, (fun -(k,1))) in (fun 10)"))
      (cps-a-program
        (cps-let-exp
          'x
          (cps-const-exp 3)
          (cps-letrec-exp
            '(fun)
            (list '(k k%00))
            (list (cps-if-exp
                    (cps-zero?-exp
                      (cps-diff-exp
                        (cps-var-exp 'k)
                        (cps-var-exp 'x)))
                    (cps-call-exp
                      (cps-var-exp 'fun)
                      (list (cps-const-exp 1) (cps-var-exp 'k%00)))
                    (cps-call-exp
                      (cps-var-exp 'fun)
                      (list (cps-diff-exp
                              (cps-var-exp 'k)
                              (cps-const-exp 1))
                       (cps-proc-exp
                         '(var%10)
                         (cps-call-exp
                           (cps-var-exp 'k%00)
                           (list (cps-diff-exp
                                   (cps-const-exp 1)
                                   (cps-var-exp 'var%10)))))))))
            (cps-call-exp
              (cps-var-exp 'fun)
              (list (cps-const-exp 10)
                    (cps-proc-exp
                      '(var%9)
                      (simple-exp->exp (cps-var-exp 'var%9)))))))))
    ))

(run-tests cps-transform-test)
