#lang eopl

(require "../libs/common.rkt")
(require "cps-in-spec.rkt")
(require "cps-out-spec.rkt")
(require "cps-transform.rkt")
(require rackunit)
(require rackunit/text-ui)

(define cps-transform-test
  (test-suite
    "cps-transform-test"
    (test-equal? "3"
                 (cps-of-program (scan-parse "3"))
                 (cps-a-program (simple-exp->exp (cps-const-exp 3))))
    (test-equal? "a"
                 (cps-of-program (scan-parse "a"))
                 (cps-a-program (simple-exp->exp (cps-var-exp 'a))))
    (test-equal? "-(3,a)"
                 (cps-of-program (scan-parse "-(3,a)"))
                 (cps-a-program (simple-exp->exp (cps-diff-exp (cps-const-exp 3) (cps-var-exp 'a)))))
    (test-equal? "proc (x) -(3,x)"
                 (cps-of-program (scan-parse "proc (x) -(3,x)"))
                 (cps-a-program (simple-exp->exp
                                  (cps-proc-exp
                                    '(x k%00)
                                    (cps-call-exp (cps-var-exp 'k%00) (list (cps-diff-exp (cps-const-exp 3) (cps-var-exp 'x))))))))
    (test-equal? "zero?((f 3))"
                 (cps-of-program (scan-parse "zero?((f 3))"))
                 (cps-a-program
                   (cps-call-exp
                     (cps-var-exp 'f)
                     (list (cps-const-exp 3)
                           (cps-proc-exp
                             '(var%2)
                             (cps-call-exp
                               (cps-proc-exp
                                 '(var%1)
                                 (simple-exp->exp (cps-var-exp 'var%1)))
                               (list (cps-zero?-exp (cps-var-exp 'var%2)))))))))
    (test-equal? "let x = 3 in -(x,3)"
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
    (test-equal?
      "letrec fun (x) = x in 3"
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
              '(var%1)
              (simple-exp->exp (cps-var-exp 'var%1)))
            (list (cps-const-exp 3))))))
    (test-equal?
      "if (f x) then (g y) else (h z)"
      (cps-of-program (scan-parse "if (f x) then (g y) else (h z)"))
      (cps-a-program
        (cps-call-exp
          (cps-var-exp 'f)
          (list (cps-var-exp 'x)
                (cps-proc-exp
                  '(var%2)
                  (cps-if-exp
                    (cps-var-exp 'var%2)
                    (cps-call-exp
                      (cps-var-exp 'g)
                      (list (cps-var-exp 'y)
                            (cps-proc-exp
                              '(var%1)
                              (simple-exp->exp (cps-var-exp 'var%1)))))
                    (cps-call-exp
                      (cps-var-exp 'h)
                      (list (cps-var-exp 'z)
                            (cps-proc-exp
                              '(var%1)
                              (simple-exp->exp (cps-var-exp 'var%1)))))))))))
    (test-equal?
      "(f (g x) (h (l y)))"
      (cps-of-program (scan-parse "(f (g x) (h (l y)))"))
      (cps-a-program
        (cps-call-exp
          (cps-var-exp 'g)
          (list (cps-var-exp 'x)
                (cps-proc-exp
                  '(var%2)
                  (cps-call-exp
                    (cps-var-exp 'l)
                    (list (cps-var-exp 'y)
                          (cps-proc-exp
                            '(var%4)
                            (cps-call-exp
                              (cps-var-exp 'h)
                              (list (cps-var-exp 'var%4)
                                    (cps-proc-exp
                                      '(var%3)
                                      (cps-call-exp
                                        (cps-var-exp 'f)
                                        (list
                                          (cps-var-exp 'var%2)
                                          (cps-var-exp 'var%3)
                                          (cps-proc-exp
                                            '(var%1)
                                            (simple-exp->exp
                                              (cps-var-exp 'var%1))))))))))))))))
    (test-equal?
      "let x = 3 in letrec fun (k) = if zero?(-(k,x)) then (fun 1) else -(1, (fun -(k,1))) in (fun 10)"
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
                              '(var%2)
                              (cps-call-exp
                                (cps-var-exp 'k%00)
                                (list (cps-diff-exp
                                        (cps-const-exp 1)
                                        (cps-var-exp 'var%2)))))))))
            (cps-call-exp
              (cps-var-exp 'fun)
              (list (cps-const-exp 10)
                    (cps-proc-exp
                      '(var%1)
                      (simple-exp->exp (cps-var-exp 'var%1)))))))))
    ))

(run-tests cps-transform-test)
