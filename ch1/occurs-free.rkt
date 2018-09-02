#lang eopl

(define occurs-free?
  (lambda (var expr)
    (cond
      ((symbol? expr) (eqv? var expr))
      ((eqv? (car expr) 'lambda)
       (and
        (not (eqv? var (car (cadr expr))))
        (occurs-free? var (caddr expr))))
      (else
        (or
          (occurs-free? var (car expr))
          (occurs-free? var (cadr expr)))))))

(provide occurs-free?)
