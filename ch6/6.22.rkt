#lang eopl

(define to-apply-cont-form
  (lambda (k-exp simple-xp)
    (cases SimpleExpression k-exp
           (csp-proc-exp (vars body)
                         (cps-let-exp
                           (car vars)
                           simple-exp
                           body))
           (else
             (cps-call-exp k-exp (list simple-exp))))))
