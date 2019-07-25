#lang racket
(require "parenthec.rkt")

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

(define apply-env-ds
  (lambda (e address)
    (union-case e env
                [(empty) (error 'value-of-cps "unbound identifier")]
                [(extend val saved-env)
                 (if (zero? address) val (apply-env saved-env (sub1 address)))])))

; Closure
(define-union closure
              (procedure body env))

(define apply-closure-ds
  (lambda (p v cont)
    (union-case p closure
                [(procedure body env)
                 (value-of-cps body (env_extend v env) (kt_closure cont))])))

; Continuation
(struct empty-k-ds ())

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

(define apply-k-ds
  (lambda (cont v)
    (union-case cont kt
                [(empty) v]
                [(mult x2 saved-env saved-cont)
                 (value-of-cps x2 saved-env (kt_mult-inner v saved-cont))]
                [(mult-inner v1 saved-cont)
                 (apply-k-ds saved-cont (* v1 v))]
                [(sub1 saved-cont)
                 (apply-k-ds saved-cont (- v 1))]
                [(zero saved-cont)
                 (apply-k-ds saved-cont (zero? v))]
                [(if conseq alt saved-env saved-cont)
                 (if v
                     (value-of-cps conseq saved-env (kt_if-conseq saved-cont))
                     (value-of-cps alt saved-env (kt_if-alt saved-cont)))]
                [(if-conseq saved-cont) (apply-k-ds saved-cont v)]
                [(if-alt saved-cont) (apply-k-ds saved-cont v)]
                [(letcc saved-cont)
                 (apply-k-ds saved-cont v)]
                [(throw saved-env saved-cont v-exp)
                 (value-of-cps v-exp saved-env (kt_throw-inner saved-cont v))]
                [(throw-inner saved-cont v1) (apply-k-ds v1 v)]
                [(let saved-env saved-cont body)
                 (value-of-cps body
                               (env_extend v saved-env)
                               (kt_let-inner saved-cont))]
                [(let-inner saved-cont) (apply-k-ds saved-cont v)]
                [(app saved-env saved-cont rand)
                 (value-of-cps rand saved-env (kt_app-inner v saved-cont))]
                [(app-inner v1 saved-cont)
                 (apply-closure v1 v saved-cont)]
                [(closure saved-cont)
                 (apply-k-ds saved-cont v)]
                )))

(define apply-k apply-k-ds)
(define apply-closure apply-closure-ds)
(define empty-k empty-k-ds)
(define apply-env apply-env-ds)

(define value-of-cps
  (lambda (expr^ env cont)
    (union-case expr^ expr
                [(const val) (apply-k cont val)]
                [(mult x1 x2)
                 (value-of-cps x1 env (kt_mult x2 env cont))]
                [(sub1 x) (value-of-cps x env (kt_sub1 cont))]
                [(zero x) (value-of-cps x env (kt_zero cont))]
                [(if test conseq alt) (value-of-cps test env (kt_if conseq alt env cont))]
                [(letcc body) (value-of-cps body (env_extend cont env) (kt_letcc cont))]
                [(throw k-exp v-exp) (value-of-cps k-exp env (kt_throw env cont v-exp))]
                [(let e body) (value-of-cps e env (kt_let env cont body))]
                [(lambda body)
                 (apply-k cont (closure_procedure body env))]
                [(app rator rand) (value-of-cps rator env (kt_app env cont rand))]
                [(var address) (apply-k cont (apply-env env address))])))

(define main 
  (lambda ()
    (value-of-cps 
      (expr_let 
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
          (expr_const 5)))
      (env_empty)
      (kt_empty))))

(main)
