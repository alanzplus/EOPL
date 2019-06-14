#lang eopl

(define identifier?
  (lambda (idf) (not (eqv? idf 'lambda))))
        
(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp)))
