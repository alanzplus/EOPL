#lang eopl

(define empty-env
  (lambda ()
    (lambda (search-var)
      (eopl:error "No binding found for ~s" search-var))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(provide empty-env)
(provide extend-env)
(provide apply-env)
