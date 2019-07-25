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

(define apply-env
  (lambda (e address)
    (union-case e env
                [(empty) (error 'value-of-cps "unbound identifier")]
                [(extend val saved-env)
                 (if (zero? address) val
                     (let* ([e saved-env]
                            [address (sub1 address)])
                       (apply-env e address)))])))

(define-union closure
              (procedure body env))

(define apply-closure
  (lambda (p v cont)
    (union-case p closure
                [(procedure body env)
                 (let* ([expr^ body]
                        [env^ (env_extend v env)]
                        [cont^ (kt_closure cont)])
                   (value-of-cps expr^ env^ cont^))])))

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

(define apply-k
  (lambda (cont v)
    (union-case cont kt
                [(empty) v]
                [(mult x2 saved-env saved-cont)
                 (let ([expr^ x2]
                       [env^ saved-env]
                       [cont^ (kt_mult-inner v saved-cont)])
                   (value-of-cps expr^ env^ cont^))]
                [(mult-inner v1 saved-cont)
                 (let* ([cont saved-cont]
                        [v (* v1 v)])
                   (apply-k cont v))]
                [(sub1 saved-cont)
                 (let* ([cont saved-cont]
                        [v (- v 1)])
                   (apply-k cont v))]
                [(zero saved-cont)
                 (let* ([cont saved-cont]
                        [v (zero? v)])
                   (apply-k cont v))]
                [(if conseq alt saved-env saved-cont)
                 (if v
                     (let* ([expr^ conseq]
                            [env^ saved-env]
                            [cont^ (kt_if-conseq saved-cont)])
                       (value-of-cps expr^ env^ cont^))
                     (let* ([expr^ alt]
                            [env^ saved-env]
                            [cont^ (kt_if-alt saved-cont)])
                       (value-of-cps expr^ env^ cont^)))]
                [(if-conseq saved-cont)
                 (let* ([cont saved-cont]
                        [v v])
                   (apply-k cont v))]
                [(if-alt saved-cont)
                 (let* ([cont saved-cont]
                        [v v])
                   (apply-k cont v))]
                [(letcc saved-cont)
                 (let* ([cont saved-cont]
                        [v v])
                   (apply-k cont v))]
                [(throw saved-env saved-cont v-exp)
                 (let* ([expr^ v-exp]
                        [env^ saved-env]
                        [cont^ (kt_throw-inner saved-cont v)])
                   (value-of-cps expr^ env^ cont^))]
                [(throw-inner saved-cont v1)
                 (let* ([cont v1]
                        [v v])
                   (apply-k cont v))]
                [(let saved-env saved-cont body)
                 (let* ([expr^ body]
                        [env^ (env_extend v saved-env)]
                        [cont^ (kt_let-inner saved-cont)])
                   (value-of-cps expr^ env^ cont^))]
                [(let-inner saved-cont)
                 (let* ([cont saved-cont]
                        [v v])
                   (apply-k cont v))]
                [(app saved-env saved-cont rand)
                 (let* ([expr^ rand]
                        [env^ saved-env]
                        [cont^ (kt_app-inner v saved-cont)])
                   (value-of-cps expr^ env^ cont^))]
                [(app-inner v1 saved-cont)
                 (let* ([p v1]
                        [v v]
                        [cont saved-cont])
                   (apply-closure p v cont))]
                [(closure saved-cont)
                 (let* ([cont saved-cont]
                        [v v])
                   (apply-k cont v))]
                )))

(define value-of-cps
  (lambda (expr^ env^ cont^)
    (union-case expr^ expr
                [(const val)
                 (let* ([cont cont^]
                        [v val])
                   (apply-k cont v))]
                [(mult x1 x2)
                 (let* ([expr^ x1]
                        [env^ env^]
                        [cont^ (kt_mult x2 env^ cont^)])
                   (value-of-cps expr^ env^ cont^))]
                [(sub1 x)
                 (let* ([expr^ x]
                        [env^ env^]
                        [cont^ (kt_sub1 cont^)])
                   (value-of-cps expr^ env^ cont^))]
                [(zero x)
                 (let* ([expr^ x]
                        [env^ env^]
                        [cont^ (kt_zero cont^)])
                   (value-of-cps expr^ env^ cont^))]
                [(if test conseq alt)
                 (let* ([expr^ test]
                        [conseq conseq]
                        [alt alt]
                        [env^ env^]
                        [cont^ (kt_if conseq alt env^ cont^)])
                   (value-of-cps expr^ env^ cont^))]
                [(letcc body)
                 (let* ([expr^ body]
                        [env^ (env_extend cont^ env^)]
                        [cont^ (kt_letcc cont^)])
                   (value-of-cps expr^ env^ cont^))]
                [(throw k-exp v-exp)
                 (let* ([expr^ k-exp]
                        [env^ env^]
                        [cont^ (kt_throw env^ cont^ v-exp)])
                   (value-of-cps expr^ env^ cont^))]
                [(let e body)
                 (let* ([expr^ e]
                        [body body]
                        [env^ env^]
                        [cont^ (kt_let env^ cont^ body)])
                   (value-of-cps expr^ env^ cont^))]
                [(lambda body)
                 (let* ([cont cont^]
                        [v (closure_procedure body env^)])
                   (apply-k cont v))]
                [(app rator rand)
                 (let* ([expr^ rator]
                        [env^ env^]
                        [cont^ (kt_app env^ cont^ rand)])
                   (value-of-cps expr^ env^ cont^))]
                [(var address)
                 (let* ([cont cont^]
                        [v (let* ([e env^]
                                  [address address])
                             (apply-env e address))])
                   (apply-k cont v))])))

(define main 
  (lambda ()
    (let* ([expr^ (expr_let 
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
                      (expr_const 5)))]
           [env^ (env_empty)]
           [cont^ (kt_empty)])
      (value-of-cps expr^ env^ cont^))))

(main)
