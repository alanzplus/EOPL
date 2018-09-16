#lang eopl

(define empty-env
  (lambda ()
    (cons
      (lambda (search-var)
        (eopl:error "No binding found for ~s" search-var))
      (lambda ()
        #t))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (cons
      (lambda (search-var)
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)))
      (lambda ()
        #f))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cdr env))))

(provide empty-env)
(provide empty-env?)
(provide extend-env)
(provide apply-env)
