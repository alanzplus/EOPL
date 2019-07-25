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

; Continuation
(struct empty-env-ds ())

(struct extend-env-ds (val saved-env))

(define apply-env-ds
  (lambda (x address)
    (match x
           [(extend-env-ds val saved-env)
            (if (zero? address) val (apply-env saved-env (sub1 address)))]
           [(empty-env-ds ) (error 'value-of-cps "unbound identifier")])))

; Closure
(define-union closure
              (procedure body env))

(define apply-closure-ds
  (lambda (p v cont)
    (union-case p closure
                [(procedure body env)
                 (value-of-cps body (extend-env v env) (closure-cont cont))])))

; Continuation
(struct empty-k-ds ())
(struct mult-cont-ds (x2 saved-env saved-cont))
(struct mult-cont-inner-ds (v saved-cont))
(struct sub1-cont-ds (saved-cont))
(struct zero-cont-ds (saved-cont))
(struct if-cont-ds (conseq alt saved-env saved-cont))
(struct if-conseq-cont-ds (saved-cont))
(struct if-alt-cont-ds (saved-cont))
(struct letcc-cont-ds (saved-cont))
(struct throw-cont-ds (saved-env saved-cont v-exp))
(struct throw-inner-cont-ds (saved-cont v))
(struct let-cont-ds (saved-env saved-cont body))
(struct let-inner-cont-ds (saved-cont))
(struct app-cont-ds (saved-env saved-cont rand))
(struct app-inner-cont-ds (v saved-cont))
(struct closure-cont (saved-cont))

(define apply-k-ds
  (lambda (cont v)
    (match cont
           [(empty-k-ds) v]
           [(mult-cont-ds x2 saved-env saved-cont)
            (value-of-cps x2 saved-env (mult-cont-inner-ds v saved-cont))]
           [(mult-cont-inner-ds v1 saved-cont)
            (apply-k-ds saved-cont (* v1 v))]
           [(sub1-cont-ds saved-cont)
            (apply-k-ds saved-cont (- v 1))]
           [(zero-cont-ds saved-cont)
            (apply-k-ds saved-cont (zero? v))]
           [(if-cont-ds conseq alt saved-env saved-cont)
            (if v
                (value-of-cps conseq saved-env (if-conseq-cont-ds saved-cont))
                (value-of-cps alt saved-env (if-alt-cont-ds saved-cont)))]
           [(if-conseq-cont-ds saved-cont) (apply-k-ds saved-cont v)]
           [(if-alt-cont-ds saved-cont) (apply-k-ds saved-cont v)]
           [(letcc-cont-ds saved-cont)
            (apply-k-ds saved-cont v)]
           [(throw-cont-ds saved-env saved-cont v-exp)
            (value-of-cps v-exp saved-env (throw-inner-cont-ds saved-cont v))]
           [(throw-inner-cont-ds saved-cont v1) (apply-k-ds v1 v)]
           [(let-cont-ds saved-env saved-cont body)
            (value-of-cps body
                          (extend-env v saved-env)
                          (let-inner-cont-ds saved-cont))]
           [(let-inner-cont-ds saved-cont) (apply-k-ds saved-cont v)]
           [(app-cont-ds saved-env saved-cont rand)
            (value-of-cps rand saved-env (app-inner-cont-ds v saved-cont))]
           [(app-inner-cont-ds v1 saved-cont)
            (apply-closure v1 v saved-cont)]
           [(closure-cont saved-cont)
            (apply-k-ds saved-cont v)]
           )))

; Bind to different implementations for environment and continuation
(define mult-cont mult-cont-ds)
(define sub1-cont sub1-cont-ds)
(define zero-cont zero-cont-ds)
(define if-cont if-cont-ds)
(define letcc-cont letcc-cont-ds)
(define throw-cont throw-cont-ds)
(define let-cont let-cont-ds)
(define app-cont app-cont-ds)
(define apply-k apply-k-ds)
(define apply-closure apply-closure-ds)
(define empty-k empty-k-ds)

(define empty-env empty-env-ds)
(define extend-env extend-env-ds)
(define apply-env apply-env-ds)

(define value-of-cps
  (lambda (expr^ env cont)
    (union-case expr^ expr
                [(const val) (apply-k cont val)]
                [(mult x1 x2)
                 (value-of-cps x1 env (mult-cont x2 env cont))]
                [(sub1 x) (value-of-cps x env (sub1-cont cont))]
                [(zero x) (value-of-cps x env (zero-cont cont))]
                [(if test conseq alt) (value-of-cps test env (if-cont conseq alt env cont))]
                [(letcc body) (value-of-cps body (extend-env cont env) (letcc-cont cont))]
                [(throw k-exp v-exp) (value-of-cps k-exp env (throw-cont env cont v-exp))]
                [(let e body) (value-of-cps e env (let-cont env cont body))]
                [(lambda body)
                 (apply-k cont (closure_procedure body env))]
                [(app rator rand) (value-of-cps rator env (app-cont env cont rand))]
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
      (empty-env)
      (empty-k))))

(main)
