#lang eopl

(define unknown-action
  (lambda (exp-type action)
    (eopl:error "Cannot apply ~s to ~s" action exp-type)))

(define apply-predicate
  (lambda (predicate)
    (lambda (lc-exp)
      ((cadr lc-exp) predicate))))

(define apply-extractor
  (lambda (extractor)
    (lambda (lc-exp)
    ((car lc-exp) extractor))))

(define var-exp
  (lambda (var)
    (list
      (lambda (action)
        (if (eqv? action 'var-exp->var)
            var
            (unknown-action 'var-exp action)))
      (lambda (action)
        (if (eqv? action 'var-exp?)
            #t
            #f)))))

(define var-exp?
  (apply-predicate 'var-exp?))

(define var-exp->var
  (apply-extractor 'var-exp->var))

(define lambda-exp
  (lambda (var lc-exp)
    (list
      (lambda (action)
        (cond ((eqv? action 'lambda-exp->bound-var) var)
              ((eqv? action 'lambda-exp->body) lc-exp)
              (else (unknown-action 'lambda-exp action))))
      (lambda (action)
        (if (eqv? action 'lambda-exp?)
            #t
            #f)))))

(define lambda-exp?
  (apply-predicate 'lambda-exp?))

(define lambda-exp->bound-var
  (apply-extractor 'lambda-exp->bound-var))

(define lambda-exp->body
  (apply-extractor 'lambda-exp->body))

(define app-exp
  (lambda (lc-exp-1 lc-exp-2)
    (list
      (lambda (action)
        (cond ((eqv? action 'app-exp->rator) lc-exp-1)
              ((eqv? action 'app-exp->rand) lc-exp-2)
              (else (unknown-action 'app-exp action))))
      (lambda (action)
        (if (eqv? action 'app-exp?)
            #t
            #f)))))

(define app-exp?
  (apply-predicate 'app-exp?))

(define app-exp->rator
  (apply-extractor 'app-exp->rator))

(define app-exp->rand
  (apply-extractor 'app-exp->rand))

(provide var-exp)
(provide var-exp?)
(provide var-exp->var)
(provide lambda-exp)
(provide lambda-exp?)
(provide lambda-exp->bound-var)
(provide lambda-exp->body)
(provide app-exp)
(provide app-exp?)
(provide app-exp->rator)
(provide app-exp->rand)
