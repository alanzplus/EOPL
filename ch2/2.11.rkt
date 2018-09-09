#lang eopl

(define empty-env
  (lambda () '()))

(define empty-env?
  (lambda (env) (null? env)))

(define extend-env
  (lambda (var val env)
    (cons (list (list var) (list val)) env)))

(define extend-env*
  (lambda (var-lst val-lst env)
    (cons (list var-lst val-lst) env)))

(define apply-env
  (lambda (env var)
    (if (null? env)
        (eopl:error "No binding for ~s" var)
        (let ((var-lst (car (car env)))
              (val-lst (cadr (car env))))
             (let ((idx (find-idx var-lst var 0)))
              (if (= -1 idx)
                  (apply-env (cdr env) var)
                  (get-val val-lst idx)))))))

(define find-idx
  (lambda (lst var idx)
    (if (null? lst)
        -1
        (if (eqv? (car lst) var)
            idx
            (find-idx (cdr lst) var (+ idx 1))))))

(define get-val
  (lambda (lst idx)
    (if (= 0 idx)
        (car lst)
        (get-val (cdr lst) (- idx 1)))))

(provide empty-env?)
(provide empty-env)
(provide extend-env)
(provide extend-env*)
(provide apply-env)
;(provide has-binding?)
