#lang eopl

(provide empty-env)
(provide extend-env)
(provide apply-env)
(provide has-binding?)

(define value?
  (lambda (val)
    #t))

; Env-exp ::= (empty-env)
;         ::= (extend-env Identifier Scheme-value Env-exp)
(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
    (var symbol?)
    (val value?)
    (env env-exp?)))

(define apply-env
  (lambda (var env)
    (cases env-exp env
      (empty-env () (eopl:error "cannot find ~s" var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? saved-var var)
            saved-val
            (apply-env var saved-env))))))

(define has-binding?
  (lambda (var env)
    (cases env-exp env
      (empty-env () #f)
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? saved-var var)
            #t
            (has-binding? var saved-env))))))
