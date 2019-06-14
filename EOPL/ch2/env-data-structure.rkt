#lang eopl

(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (val var env)
    (list 'extend-env val var env)))

(define apply-env
  (lambda (env var)
    (cond
      ((eqv? (car env) 'empty-env)
        (eopl:error "No bindg for ~s" var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
            (if (eqv? saved-var var)
                saved-val
                (apply-env saved-env var))))
      (else
        (eopl:error "It is not a valid environment\n")))))

(provide empty-env)
(provide extend-env)
(provide apply-env)
