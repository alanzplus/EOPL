#lang racket
(require "parenthec.rkt")

(define-registers expr^ address^ env^ cont^ v^ closure^)
(define-program-counter pc)

(define-union expr
              (const cexp)
              (var n)
              (if test conseq alt)
              (mult nexp1 nexp2)
              (sub1 nexp)
              (zero nexp)
              (letcc body)
              (throw kexp vexp)
              (let exp body)              
              (lambda body)
              (app rator rand))

(define-union env
              (empty)
              (extend val saved-env))

(define-label apply-env
              (union-case env^ env
                          [(empty) (error 'value-of-cps "unbound identifier")]
                          [(extend val saved-env)
                           (if (zero? address^)
                               (begin
                                 (set! cont^ cont^)
                                 (set! v^ val)
                                 (apply-k))
                               (begin (set! env^ saved-env)
                                      (set! address^ (sub1 address^))
                                      (set! cont^ cont^)
                                      (apply-env)))]))

(define-union closure
              (procedure body env))

(define-label apply-closure
              (union-case closure^ closure
                          [(procedure body env)
                           (begin (set! expr^ body)
                                  (set! env^ (env_extend v^ env))
                                  (set! cont^ (kt_closure cont^))
                                  (value-of-cps))]))

(define-union kt
              (empty)
              (mult x2 saved-env saved-cont)
              (mult-inner v saved-cont)
              (sub1 saved-cont)
              (zero saved-cont)
              (if conseq alt saved-env saved-cont)
              (if-conseq saved-cont)
              (if-alt saved-cont)
              (letcc saved-cont)
              (throw saved-env saved-cont v-exp)
              (throw-inner saved-cont v)
              (let saved-env saved-cont body)
              (let-inner saved-cont)
              (app saved-env saved-cont rand)
              (app-inner v saved-cont)
              (closure saved-cont))

(define-label apply-k
              (union-case cont^ kt
                          [(empty) v^]
                          [(mult x2 saved-env saved-cont)
                           (begin (set! expr^ x2)
                                  (set! env^ saved-env)
                                  (set! cont^ (kt_mult-inner v^ saved-cont))
                                  (value-of-cps))]
                          [(mult-inner v1 saved-cont)
                           (begin (set! cont^ saved-cont)
                                  (set! v^ (* v1 v^))
                                  (apply-k))]
                          [(sub1 saved-cont)
                           (begin (set! cont^ saved-cont)
                                  (set! v^ (- v^ 1))
                                  (apply-k))]
                          [(zero saved-cont)
                           (begin (set! cont^ saved-cont)
                                  (set! v^ (zero? v^))
                                  (apply-k))]
                          [(if conseq alt saved-env saved-cont)
                           (if v^
                               (begin (set! expr^ conseq)
                                      (set! env^ saved-env)
                                      (set! cont^ (kt_if-conseq saved-cont))
                                      (value-of-cps))
                               (begin (set! expr^ alt)
                                      (set! env^ saved-env)
                                      (set! cont^ (kt_if-alt saved-cont))
                                      (value-of-cps)))]
                          [(if-conseq saved-cont)
                           (begin (set! cont^ saved-cont)
                                  (set! v^ v^)
                                  (apply-k))]
                          [(if-alt saved-cont)
                           (begin (set! cont^ saved-cont)
                                  (set! v^ v^)
                                  (apply-k))]
                          [(letcc saved-cont)
                           (begin (set! cont^ saved-cont)
                                  (set! v^ v^)
                                  (apply-k))]
                          [(throw saved-env saved-cont v-exp)
                           (begin (set! expr^ v-exp)
                                  (set! env^ saved-env)
                                  (set! cont^ (kt_throw-inner saved-cont v^))
                                  (value-of-cps))]
                          [(throw-inner saved-cont v1)
                           (begin (set! cont^ v1)
                                  (set! v^ v^)
                                  (apply-k))]
                          [(let saved-env saved-cont body)
                           (begin (set! expr^ body)
                                  (set! env^ (env_extend v^ saved-env))
                                  (set! cont^ (kt_let-inner saved-cont))
                                  (value-of-cps))]
                          [(let-inner saved-cont)
                           (begin 
                             (set! cont^ saved-cont)
                             (set! v^ v^)
                             (apply-k))]
                          [(app saved-env saved-cont rand)
                           (begin (set! expr^ rand)
                                  (set! env^ saved-env)
                                  (set! cont^ (kt_app-inner v^ saved-cont))
                                  (value-of-cps))]
                          [(app-inner v1 saved-cont)
                           (begin (set! closure^ v1)
                                  (set! v^ v^)
                                  (set! cont^ saved-cont)
                                  (apply-closure))]
                          [(closure saved-cont)
                           (begin (set! cont^ saved-cont)
                                  (set! v^ v^)
                                  (apply-k))]))

(define-label value-of-cps
              (union-case expr^ expr
                          [(const val)
                           (begin (set! cont^ cont^)
                                  (set! v^ val)
                                  (apply-k))]
                          [(mult x1 x2)
                           (begin (set! expr^ x1)
                                  (set! env^ env^)
                                  (set! cont^ (kt_mult x2 env^ cont^))
                                  (value-of-cps))]
                          [(sub1 x)
                           (begin (set! expr^ x)
                                  (set! env^ env^)
                                  (set! cont^ (kt_sub1 cont^))
                                  (value-of-cps))]
                          [(zero x)
                           (begin (set! expr^ x)
                                  (set! env^ env^)
                                  (set! cont^ (kt_zero cont^))
                                  (value-of-cps))]
                          [(if test conseq alt)
                           (begin (set! expr^ test)
                                  (set! conseq conseq)
                                  (set! alt alt)
                                  (set! env^ env^)
                                  (set! cont^ (kt_if conseq alt env^ cont^))
                                  (value-of-cps))]
                          [(letcc body)
                           (begin (set! expr^ body)
                                  (set! env^ (env_extend cont^ env^))
                                  (set! cont^ (kt_letcc cont^))
                                  (value-of-cps))]
                          [(throw k-exp v-exp)
                           (begin (set! expr^ k-exp)
                                  (set! env^ env^)
                                  (set! cont^ (kt_throw env^ cont^ v-exp))
                                  (value-of-cps))]
                          [(let e body)
                           (begin (set! expr^ e)
                                  (set! env^ env^)
                                  (set! cont^ (kt_let env^ cont^ body))
                                  (value-of-cps))]
                          [(lambda body)
                           (begin (set! cont^ cont^)
                                  (set! v^ (closure_procedure body env^))
                                  (apply-k))]
                          [(app rator rand)
                           (begin (set! expr^ rator)
                                  (set! env^ env^)
                                  (set! cont^ (kt_app env^ cont^ rand))
                                  (value-of-cps))]
                          [(var address)
                           (begin (set! env^ env^)
                                  (set! address^ address)
                                  (set! cont^ cont^)
                                  (apply-env))]))

(define-label main 
              (begin (set! expr^ (expr_let 
                                   (expr_lambda
                                     (expr_lambda 
                                       (expr_if
                                         (expr_zero (expr_var 0))
                                         (expr_const 1)
                                         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
                                   (expr_mult
                                     (expr_letcc
                                       (expr_app
                                         (expr_app (expr_var 1) (expr_var 1))
                                         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
                                     (expr_const 5))))
                     (set! env^ (env_empty))
                     (set! cont^ (kt_empty))
                     (value-of-cps)))

(main)
