#lang eopl

(define empty-env
  (lambda () '()))

(define empty-env?
  (lambda (env) (null? env)))

(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))

(define extend-env*
  (lambda (var-lst val-lst env)
    (if (and (null? var-lst) (null? val-lst))
        env
        (let ((var (car var-lst))
              (val (car val-lst)))
            (cons
              (list var val)
              (extend-env* (cdr var-lst) (cdr val-lst) env))))))

(define apply-env
  (lambda (env var)
    (if (null? env)
        (eopl:error "No binding for ~s" var)
        (let ((saved-var (car (car env)))
              (saved-val (cadr (car env))))
             (if (eqv? saved-var var)
                 saved-val
                 (apply-env (cdr env) var))))))

(define has-binding?
  (lambda (env var)
    (if (null? env)
        #f
        (let ((saved-var (car (car env)))
              (saved-val (cadr (car env))))
             (if (eqv? saved-var var)
                 #t
                 (has-binding? (cdr env) var))))))

(provide empty-env?)
(provide empty-env)
(provide extend-env)
(provide extend-env*)
(provide apply-env)
(provide has-binding?)
