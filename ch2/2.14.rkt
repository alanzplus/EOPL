#lang eopl

(define empty-env
  (lambda ()
    (list
      (lambda (search-var)
        (eopl:error "No binding found for ~s" search-var))
      (lambda ()
        #t)
      (lambda (search-var) #f))))

(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
      (lambda (search-var)
        (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var)))
      (lambda ()
        #f)
      (lambda (search-var)
        (if (eqv? search-var saved-var)
            #t
            (has-binding? search-var saved-env))))))
        
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cadr env))))

(define has-binding?
  (lambda (var env)
    ((car (cdr (cdr env))) var)))

(provide empty-env)
(provide empty-env?)
(provide extend-env)
(provide apply-env)
(provide has-binding?)
